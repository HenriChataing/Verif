(** Run the analysis. *)

open Apron
open Expressions
open Bexpr
open Expr

open Utils
open Vars
open Types
open Positions
open Horn
open Generics


(** Types modified to hold Apron environments and expressions. *)
type clause =
  (Environment.t, Environment.t,
   ((Tcons1.t, var * Texpr1.t array) either Bexpr.t) list) gen_clause

type predicate =
  (Var.t array, Environment.t,
   clause) gen_predicate

type script = (predicate, clause) gen_script


(** Create an environment from a list of variables. *)
let make_environment (vs: var list): Environment.t =
  (* Int and real variables. *)
  let ints, reals = List.partition (fun v -> v.typ != TyFloat) vs in

  let ints = List.map (fun v -> Var.of_string (vname v)) ints
  and reals = List.map (fun v -> Var.of_string (vname v)) reals in
  let aryi = Array.of_list ints
  and aryf = Array.of_list reals in
  Environment.make aryi aryf


(** Convert an expression into an Apron expression. *)
let texpr_of_expr (env: Environment.t) (e: Expr.t): Texpr1.t =
  let rec convert (e: Expr.t): Texpr1.expr =
    match e with
    | Var (_, v) -> Texpr1.Var (Var.of_string (vname v))
    | Prim (_, p) -> Texpr1.Cst (Primitive.to_coeff p)
    | Unary (_, "-", e) ->
        Texpr1.Unop (
          Texpr1.Neg, convert e,
          typ_of_typ (typeof e),
          Texpr1.Near
        )
    | Unary (pos, op, _) ->
        Errors.fatal' pos ("The operator '" ^ op ^ "' is not supported in Apron.")
    | Binary (pos, op, e0, e1) ->
        let op = match op with
          | "+" -> Texpr1.Add | "-" -> Texpr1.Sub
          | "*" -> Texpr1.Mul | "/" -> Texpr1.Div
          | _ -> Errors.fatal' pos ("The operator '" ^ op ^ "' is not supported in Apron.")
        in
        Texpr1.Binop (
          op, convert e0, convert e1,
          typ_of_typ (typeof e0),
          Texpr1.Near
        )
    | Predicate (pos, _,_) ->
        Errors.fatal' pos "Unexpected predicate in expression."
  in
  Texpr1.of_expr env (convert e)


(** Convert an expression to a boolean expression. *)
let rec bexpr_of_expr
    (env: Environment.t) (e: Expr.t)
  : (Tcons1.t, var * Texpr1.t array) either Bexpr.t =
  match e with
  | Prim (_, Primitive.Bool b) -> if b then Top else Bot
  | Unary (_, "not", e) -> bexpr_of_expr env (enot e)
  | Binary (_, "&&", e0, e1) -> band (bexpr_of_expr env e0) (bexpr_of_expr env e1)
  | Binary (_, "||", e0, e1) -> bor (bexpr_of_expr env e0) (bexpr_of_expr env e1)
  | Binary (p, op, e0, e1) ->
      let e = texpr_of_expr env (Binary (p, "-", e0, e1))
      and ne = texpr_of_expr env (Binary (p, "-", e1, e0)) in
      let comps = match op with
        | "==" -> [Tcons1.EQ, e]
        | "<>" -> [Tcons1.SUP, e; Tcons1.SUP, ne]
        | ">=" -> [Tcons1.SUPEQ, e]
        | ">" -> [Tcons1.SUP, e]
        | "<=" -> [Tcons1.SUPEQ, ne]
        | "<" -> [Tcons1.SUP, ne]
        | _ -> Errors.fatal' p ("Unrecognised comparator '" ^ op ^ "'.")
      in
      let bs = List.map (fun (cmp, te) -> Atom (Left (Tcons1.make te cmp))) comps in
      begin match bs with
      | [b] -> b
      | _ -> Disj bs
      end
  | Predicate (_, c, es) ->
      let arg = List.map (texpr_of_expr env) es in
      Atom (Right (c, Array.of_list arg))
  | _ -> Errors.fatal' (Expr.position e) "This expression is not boolean"


(** Convert a tree expression to an expression. *)
let expr_of_texpr (context: var list) (te: Texpr1.t): Expr.t =
  let from_context x =
    List.find (fun v -> vname v = x) context in
  let operators = [
    (Texpr1.Add, "+"); (Texpr1.Sub, "-");
    (Texpr1.Mul, "*"); (Texpr1.Div, "/") ] in
  let rec convert te =
    match te with
    | Texpr1.Cst c -> Expr.Prim (undefined_position, Primitive.of_coeff c)
    | Texpr1.Var v -> Expr.Var (undefined_position, from_context (Var.to_string v))
    | Texpr1.Unop (Texpr1.Neg, te, _, _) -> Expr.Unary (undefined_position, "-", convert te)
    | Texpr1.Binop (op, te0, te1, _, _) ->
        let e0 = convert te0
        and e1 = convert te1 in
        begin try
          Expr.Binary (undefined_position, List.assoc op operators, e0, e1)
        with Not_found -> Errors.fatal [] "Unsupported Apron operator"
        end
    | _ -> Errors.fatal [] "Unsupported Apron expression"
  in
  convert (Texpr1.to_expr te)


(** Convert a constraint on a tree expression to an expression. *)
let expr_of_tcons (context: var list) (tc: Tcons1.t): Expr.t =
  let e = expr_of_texpr context (Tcons1.get_texpr1 tc) in
  match Tcons1.get_typ tc with
  | Tcons1.EQ -> Expr.Binary (undefined_position, "==", e,
                   Expr.Prim (undefined_position, Primitive.Int 0))
  | Tcons1.DISEQ -> Expr.Binary (undefined_position, "<>", e,
                      Expr.Prim (undefined_position, Primitive.Int 0))
  | Tcons1.SUP -> Expr.Binary (undefined_position, ">", e,
                    Expr.Prim (undefined_position, Primitive.Int 0))
  | Tcons1.SUPEQ -> Expr.Binary (undefined_position, ">=", e,
                      Expr.Prim (undefined_position, Primitive.Int 0))
  | _ -> Errors.fatal [] "Unsupported Apron constraint type"


(** Convert an invariant to a list of constraints. *)
let exprs_of_value
    (context: var list)
    (man: 'a Manager.t)
    (d: 'a Abstract1.t): Expr.t list =
  let tcs = Abstract1.to_tcons_array man d in
  let es = ref [] in
  for i=0 to (Tcons1.array_length tcs)-1 do
    es := (expr_of_tcons context (Tcons1.array_get tcs i))::!es
  done;
  !es


(** Convert a clause. *)
let convert_clause (args: Environment.t) (c: Horn.clause): clause =
  let env = make_environment c.variables in
  { cname = c.cname;
    cpos = c.cpos;
    variables = env;
    arguments = args;
    preconds = List.map (bexpr_of_expr env) c.preconds;
    negative = c.negative
  }


(** Convert a predicate. *)
let convert_predicate (p: Horn.predicate): predicate =
  if p.valid then
    let args = make_environment p.arguments' in
    { pname = p.pname;
      arguments' = Array.of_list (List.map (fun v -> Var.of_string (vname v)) p.arguments');
      environment = args;
      clauses = List.map (convert_clause args) p.clauses;
      mark = 0; widen = p.widen; head = p.head;
      children = p.children; ancestors = p.ancestors;
      fromloops = p.fromloops;
      valid = true
    }
  else
    { pname = p.pname;
      arguments' = [||]; environment = Environment.make [||] [||];
      clauses = [];
      mark = 0; widen = false; head = false;
      children = []; ancestors = []; fromloops = [];
      valid = false
    }


(** Convert a script with surface types to a script with Apron types. *)
let convert_script (s: Horn.script): script =
  { context = s.context;
    predicates = Array.map convert_predicate s.predicates;
    negatives = List.map (convert_clause (Environment.make [||] [||])) s.negatives;
    commands = s.commands
  }

(** Abstract state. Each predicate is given an abstract value. *)
type 'a abstract_state = 'a Abstract1.t array


(** Build the initial abstract state. *)
let make_initial_state
    (man: 'a Manager.t)
    (g: script): 'a abstract_state =
  let vals = List.map (fun p ->
    if p.head then
      Abstract1.top man p.environment
    else
      Abstract1.bottom man p.environment
  ) (Array.to_list g.predicates) in

  Array.of_list vals


(** Return the list of predicates appearing in a boolean expression. *)
let rec predicates_of_bexpr (b: (Tcons1.t, var * Texpr1.t array) either Bexpr.t): var list =
  match b with
  | Top | Bot -> []
  | Atom (Left _) -> [] | Atom (Right (c,_)) -> [c]
  | Conj bs -> List.concat (List.map predicates_of_bexpr bs)
  | Disj bs -> List.concat (List.map predicates_of_bexpr bs)


(** Compute the value of a boolean expression. *)
let rec evaluate_bexpr
    (man: 'a Manager.t)
    (env: Environment.t)
    (g: script)
    (state: 'a abstract_state)
    (b: (Tcons1.t,var * Texpr1.t array) either Bexpr.t): 'a Abstract1.t =
  match b with
  | Top -> Abstract1.top man env | Bot -> Abstract1.bottom man env
  | Conj bs ->
      Logger.log ~mode:"abstract-debug" "    Conj: ";
      let v = List.fold_left (fun v b ->
        let v' = evaluate_bexpr man env g state b in
        Logger.log ~mode:"abstract-debug" " "; Logger.loga ~mode:"abstract-debug" v';
        Abstract1.meet man v v' (* (evaluate_bexpr man env g state b) *)
      ) (Abstract1.top man env) bs in
      Logger.log ~mode:"abstract-debug" " = "; Logger.loga ~mode:"abstract-debug" v;
      Logger.newline ~mode:"abstract-debug" ();
      v
  | Disj bs ->
      Logger.log ~mode:"abstract-debug" "    Disj: ";
      let v = List.fold_left (fun v b ->
        let v' = evaluate_bexpr man env g state b in
        Logger.log ~mode:"abstract-debug" " "; Logger.loga ~mode:"abstract-debug" v';
        Abstract1.join man v v' (*(evaluate_bexpr man env g state b) *)
      ) (Abstract1.bottom man env) bs in
      Logger.log ~mode:"abstract-debug" " = ";
      Logger.loga ~mode:"abstrac-debug" v; Logger.newline ~mode:"abstract-debug" ();
      v
  | Atom (Left c) ->
      let ary = Tcons1.array_make env 1 in
      Tcons1.array_set ary 0 c;
      Abstract1.of_tcons_array man env ary
  | Atom (Right (p, arg)) ->
      let v = state.(p.vid) in
      let penv = g.predicates.(p.vid).environment
      and pargs = g.predicates.(p.vid).arguments' in
      let v = Abstract1.change_environment man v (Environment.lce env penv) false in
      let v = Abstract1.substitute_texpr_array man v pargs arg None in
      Logger.log ~mode:"abstract-debug" ("    Pred [" ^ g.predicates.(p.vid).pname.name ^ "]: ");
      Logger.loga ~mode:"abstract-debug" v; Logger.newline ~mode:"abstract-debug" ();
      Abstract1.change_environment man v env true


(** Compute the meet value of the preconditions of a clause. *)
let evaluate_preconds
    (man: 'a Manager.t)
    (g: script)
    (state: 'a abstract_state)
    (c: clause): 'a Abstract1.t =
  Logger.log ~mode:"abstract-debug" ("  Conj [" ^ c.cname.name ^ "]: ");
  Logger.newline ~mode:"abstract-debug" ();
  let env = c.variables in
  let v = List.fold_left (fun v b ->
    let v' = evaluate_bexpr man env g state b in
    Logger.log ~mode:"abstract-debug" "    "; Logger.loga ~mode:"abstract-debug" v';
    Logger.newline ~mode:"abstract-debug" ();
    Abstract1.meet man v v' (* (evaluate_bexpr man env g state b) *)
  ) (Abstract1.top man env) c.preconds in
  Logger.log ~mode:"abstract-debug" "  = "; Logger.loga ~mode:"abstract-debug" v;
  Logger.newline ~mode:"abstract-debug" ();
  Abstract1.change_environment man v c.arguments true


(** Display the abstract state. *)
let print_abstract_state (fmt: Format.formatter) (g: script) (state: 'a abstract_state): unit =
  for i=0 to (Array.length g.predicates)-1 do
    Format.pp_print_string fmt (g.predicates.(i).pname.name ^ ": ");
    Abstract1.print fmt state.(i);
    Format.pp_print_newline fmt ()
  done


(** Run the analysis. *)
let run_analysis
    (man: 'a Manager.t)
    (g: script): 'a abstract_state =
  Logger.log ~mode:"abstract" "### Run analysis ###"; Logger.newline ~mode:"abstract" ();
  (* The abstract state. *)
  let state = make_initial_state man g in
  (* The predicates. *)
  let preds = g.predicates in
  (* Stability flag. *)
  let stable = ref false in

  (* Update the value at one point. *)
  let update ?(dowiden: bool = true) (c: int): unit =
    let pre = List.map (evaluate_preconds man g state) preds.(c).clauses in
    let env = preds.(c).environment in
    let d = List.fold_left (fun d p -> Abstract1.join man d p) (Abstract1.bottom man env) pre in
    let d' = if preds.(c).widen && dowiden then Abstract1.widening man state.(c) d else d in

    Logger.log ~mode:"abstract" ("Update [" ^ preds.(c).pname.name ^"]: ");
    Logger.loga ~mode:"abstract" d'; Logger.newline ~mode:"abstract" ();

    if not (Abstract1.is_eq man state.(c) d') then begin
      state.(c) <- d';
      stable := false
    end
  in

  (* Loop ancestors. *)
  let loop_ancestors (loop: int) (c: int) =
    List.fold_left (fun n c' ->
      if List.mem loop preds.(c').fromloops then n else n+1) 0 preds.(c).ancestors
  in
  (* Clear the marks. *)
  let clear_marks (loop: int): unit =
    for i=0 to (Array.length preds)-1 do
      if i = loop then
        preds.(i).mark <- loop_ancestors loop i
      else if List.mem loop preds.(i).fromloops then
        preds.(i).mark <- 0
    done
  in

  (* Iterate. *)
  let rec iterate (loop: int option) (c: int): unit =
    begin match loop with
    | None ->
        Logger.log ~mode:"abstract-debug" ("Iterate: [] " ^ preds.(c).pname.name ^ "\n")
    | Some cloop ->
        Logger.log ~mode:"abstract-debug"
          ("Iterate: [" ^ preds.(cloop).pname.name ^ "] " ^ preds.(c).pname.name ^ "\n")
    end;

    preds.(c).mark <- preds.(c).mark + 1;

    (* Loop header. *)
    if preds.(c).widen then begin
      (* Reached the stop point. *)
      if loop = Some c then ()
      (* Launch another loop iteration. *)
      else if preds.(c).mark = loop_ancestors c c then begin
        update ~dowiden:false c;
        (* Widen and stabilize. *)
        let invariant = ref (Abstract1.bottom man preds.(c).environment) in
        while !invariant <> state.(c) do
          invariant := state.(c);
          Utils.iteron (fromloop g c) (iterate (Some c)) preds.(c).children;
          update c; clear_marks c
        done;
        (* Narrow. *)
        Utils.iteron (fromloop g c) (iterate (Some c)) preds.(c).children;
        update ~dowiden:false c; clear_marks c;
        (* Leave the loop. *)
        for c'=0 to (Array.length preds)-1 do
          if fromloop g c c' then
            Utils.iteron (fun c' -> not (fromloop g c c')) (iterate loop) preds.(c').children
        done
      end

    (* Not a loop header. *)
    end else begin
      (* Waiting for additional values. *)
      if preds.(c).mark < List.length preds.(c).ancestors then ()
      (* All values are here. *)
      else if preds.(c).mark = List.length preds.(c).ancestors then begin
        (* Compute the value at c. *)
        update c;
        match loop with
        | None -> List.iter (iterate loop) preds.(c).children
        | Some cloop -> Utils.iteron (fromloop g cloop) (iterate loop) preds.(c).children
      end
    end
  in

  (* Start the iteration at all header points. *)
  for i=0 to (Array.length preds)-1 do
    preds.(i).mark <- if preds.(i).head then -1 else 0;
  done;
  for i=0 to (Array.length preds)-1 do
    if preds.(i).head then iterate None i
  done;

  for i=0 to (Array.length state)-1 do
    if preds.(i).valid then begin
      Logger.log ~mode:"abstract" (preds.(i).pname.name ^ ": ");
      Logger.loga ~mode:"abstract" state.(i); Logger.newline ~mode:"abstract" ()
    end
  done;

  state


(** Check the inferred abstract state against the negative clauses. *)
let check_negatives
    (man: 'a Manager.t)
    (g: script)
    (state: 'a abstract_state): bool =
  List.for_all (fun c ->
    let d = evaluate_preconds man g state c in
    Abstract1.is_bottom man d
  ) g.negatives


(** Insert the computed invariant as preconditions of clauses relevant
  to a loop predicate. *)
let insert_invariant
    (man: 'a Manager.t)
    (state: 'a abstract_state)
    (ascript: script)
    (script: Horn.script): unit =
  for i=0 to (Array.length script.predicates)-1 do
    if script.predicates.(i).valid && script.predicates.(i).widen then begin
      let es = exprs_of_value script.predicates.(i).arguments' man state.(i) in
      List.iter (fun c -> c.preconds <- es @ c.preconds) script.predicates.(i).clauses
    end
  done


