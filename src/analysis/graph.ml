(** Build and analyze the dependency graph of the horn clauses. *)

open Apron
open Expressions
open Expr
open Bexpr

open Utils
open Vars
open Types
open Positions
open Horn


(** Modified type of clauses, where expression have been converted to Apron format. *)
type clause = {
  cname: var;
  cpos: position;
  mutable variables: Environment.t;
  mutable arguments: Environment.t;
  mutable preconds: ((Tcons1.t, var * Texpr1.t array) either Bexpr.t) list;
  negative: bool
}

type node = {
  pname: var;                  (* The name of the predicate. *)
  parguments: Var.t array;     (* The arguments of the predicate. *)
  environment: Environment.t;  (* The associated Apron environment. *)
  clauses: clause list;        (* The list of clauses whose goal is the predicate. *)

  mutable mark: int;           (* A boolean marker. *)
  mutable widen: bool;         (* Identify widening points. *)
  mutable head: bool;          (* Identify the starting points of the analysis. *)

  mutable children: int list;  (* The list of predicates that depends upon this node. *)
  mutable ancestors: int list  (* The number of dependencies. *)
}

type graph = {
  predicates: node array; (* Definition of each predicate. *)
  negatives: clause list  (* Remaining, negative clauses. *)
}


(** Create an environment from a list of variables. *)
let make_environment (vs: var list): Environment.t =
  (* Int and real variables. *)
  let ints, reals = List.partition (fun v -> v.typ != TyFloat) vs in

  let ints = List.map (fun v -> Var.of_string v.name) ints
  and reals = List.map (fun v -> Var.of_string v.name) reals in
  let aryi = Array.of_list ints
  and aryf = Array.of_list reals in
  Environment.make aryi aryf


(** Convert an expression into an Apron expression. *)
let texpr_of_expr (env: Environment.t) (e: Expr.t): Texpr1.t =
  let rec convert (e: Expr.t): Texpr1.expr =
    match e with
    | Var (_, v) -> Texpr1.Var (Var.of_string v.name)
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
      let bs = List.map (fun (cmp, te) -> Atom (Left (Tcons1.make e cmp))) comps in
      Conj bs
  | Predicate (_, c, es) ->
      let arg = List.map (texpr_of_expr env) es in
      Atom (Right (c, Array.of_list arg))
  | _ -> Errors.fatal' (Expr.position e) "This expression is not boolean"


(** Return the list of predicates appearing in a boolean expression. *)
let rec predicates_of_bexpr (b: (Tcons1.t, var * Texpr1.t array) either Bexpr.t): var list =
  match b with
  | Top | Bot -> []
  | Atom (Left _) -> [] | Atom (Right (c,_)) -> [c]
  | Conj bs -> List.concat (List.map predicates_of_bexpr bs)
  | Disj bs -> List.concat (List.map predicates_of_bexpr bs)


(** Build the graph associated with a program. *)
let build_graph (p: script): graph =
  (* Create the graph. *)
  let preds = Array.make (List.length p.context) {
    pname = dummy_var;
    parguments = [||];
    environment = Environment.make [||] [||];
    clauses = []; mark = 0; widen = false; head = false;
    children = []; ancestors = []
  } in

  (* Update the nodes. *)
  List.iter (fun cn ->
    (* Extract of the relevant clauses. *)
    let cs = List.filter (fun c ->
      not c.Horn.negative && c.Horn.cname.name = cn.name) p.Horn.clauses in
    let args = match cs with
      | [] -> []
      | c::_ -> c.Horn.arguments
    in
    let args = List.map (function
        Expr.Var (_, v) -> v
      | _ -> assert false
    ) args in

    (* Construction of the argument environment. *)
    let env = make_environment args in
    (* Conversion of the clauses. *)
    let cs = List.map (fun c ->
      let venv = make_environment c.Horn.variables in
      let pre = List.map (bexpr_of_expr venv) c.Horn.preconds in
      { cname = cn; cpos = c.Horn.cpos;
        preconds = pre; variables = venv;
        arguments = env; negative = false }
    ) cs in

    preds.(cn.vid) <- {
      pname = cn;
      environment = env;
      parguments = Array.of_list (List.map (fun v -> Var.of_string v.name) args) ;
      clauses = cs; mark = 0;
      widen = false; head = false;
      children = []; ancestors = []
    }
  ) p.context;

  (* Filter the negative clauses. *)
  let negs = List.filter (fun c -> c.Horn.negative) p.Horn.clauses in
  let negs = List.map (fun c ->
    let venv = make_environment c.Horn.variables in
    let pre = List.map (bexpr_of_expr venv) c.Horn.preconds in
    { cname = dummy_var; variables = venv;
      cpos = c.Horn.cpos;
      arguments = Environment.make [||] [||];
      preconds = pre; negative = true
    }
  ) negs in

  { predicates = preds; negatives = negs }


(** Identify a group of points cutting the loops, and mark them as widening points.
    If a file name is specified, then a dot file is produced as a side effect. *)
let identify_widening_points ?(dot: string = "") (g: graph): unit =
  (* Inserting dependencies. *)
  let add i j =
    g.predicates.(i).children <- Utils.insert j g.predicates.(i).children;
    g.predicates.(j).ancestors <- Utils.insert i g.predicates.(j).ancestors
  in
  for i=0 to (Array.length g.predicates)-1 do
    List.iter (fun c ->
      List.iter (fun b ->
        List.iter (fun j -> add j.vid i) (predicates_of_bexpr b)
      ) c.preconds
    ) g.predicates.(i).clauses
  done;
  (* Graph exploration. *)
  let rec walk (hist: int list) (i: int): unit =
    if g.predicates.(i).mark == 0 then begin
      g.predicates.(i).mark <- 1;
      List.iter (walk (i::hist)) g.predicates.(i).children
    end else if List.mem i hist then
      g.predicates.(i).widen <- true
  in
  (* Launch the exploration on all nodes with no incoming edges. *)
  for i=0 to (Array.length g.predicates)-1 do
    if g.predicates.(i).ancestors = [] then begin
      g.predicates.(i).head <- true;
      walk [] i
    end
  done;
  (* If it isn't enough, pick up other nodes until all nodes have been visited. *)
  let finished = ref false in
  while not !finished do
    finished := true;
    for i=0 to (Array.length g.predicates)-1 do
      if g.predicates.(i).mark == 0 then begin
        finished := false;
        g.predicates.(i).head <- true;
        walk [] i
      end
    done
  done;
  (* Reinitialize the markers, and update the list of children. *)
  for i=0 to (Array.length g.predicates)-1 do
    g.predicates.(i).mark <- 0
  done;
  (* Write the dot file, if needed. *)
  if dot <> "" then begin
    let fout = open_out dot in
    let f = Format.formatter_of_out_channel fout in
    let open Format in
    pp_print_string f "digraph g {"; pp_print_newline f ();
    for i=0 to (Array.length g.predicates)-1 do
      if g.predicates.(i).widen then
        fprintf f "  %i [color=red label=\"%s\" shape=box];" i g.predicates.(i).pname.name
      else if g.predicates.(i).head then
        fprintf f "  %i [color=green label=\"%s\" shape=box];" i g.predicates.(i).pname.name
      else
        fprintf f "  %i [color=blue label=\"%s\" shape=box];" i g.predicates.(i).pname.name;
      pp_print_newline f ()
    done;
    for i=0 to (Array.length g.predicates)-1 do
      List.iter (fun j ->
        fprintf f "  %i -> %i;" i j;
        pp_print_newline f ()
      ) g.predicates.(i).children
    done;
    pp_print_string f "}";
    pp_print_flush f ();
  end


(** Abstract state. Each predicate is given an abstract value. *)
type 'a abstract_state = 'a Abstract1.t array


(** Build the initial abstract state. *)
let make_initial_state
    (man: 'a Manager.t)
    (g: graph): 'a abstract_state =
  let vals = List.map (fun p ->
    if p.head then
      Abstract1.top man p.environment
    else
      Abstract1.bottom man p.environment
  ) (Array.to_list g.predicates) in

  Array.of_list vals


(** Compute the value of a boolean expression. *)
let rec evaluate_bexpr
    (man: 'a Manager.t)
    (env: Environment.t)
    (g: graph)
    (state: 'a abstract_state)
    (b: (Tcons1.t,var * Texpr1.t array) either Bexpr.t): 'a Abstract1.t =
  match b with
  | Top -> Abstract1.top man env | Bot -> Abstract1.bottom man env
  | Conj bs ->
      List.fold_left (fun v b ->
        Abstract1.meet man v (evaluate_bexpr man env g state b)
      ) (Abstract1.top man env) bs
  | Disj bs ->
      List.fold_left (fun v b ->
        Abstract1.join man v (evaluate_bexpr man env g state b)
      ) (Abstract1.bottom man env) bs
  | Atom (Left c) ->
      let ary = Tcons1.array_make env 1 in
      Tcons1.array_set ary 0 c;
      Abstract1.of_tcons_array man env ary
  | Atom (Right (p, arg)) ->
      let v = state.(p.vid) in
      let penv = g.predicates.(p.vid).environment
      and pargs = g.predicates.(p.vid).parguments in
      let v = Abstract1.change_environment man v (Environment.lce env penv) false in
      let v = Abstract1.substitute_texpr_array man v pargs arg None in
      Abstract1.change_environment man v env true


(** Compute the meet value of the list of preconditions of a clause. *)
let evaluate_preconds
    (man: 'a Manager.t)
    (g: graph)
    (state: 'a abstract_state)
    (c: clause): 'a Abstract1.t =
  let env = c.variables in
  let v = List.fold_left (fun v b ->
    Abstract1.meet man v (evaluate_bexpr man env g state b)
  ) (Abstract1.top man env) c.preconds in
  Abstract1.change_environment man v c.arguments true


(** Display the abstract state. *)
let print_abstract_state (fmt: Format.formatter) (g: graph) (state: 'a abstract_state): unit =
  for i=0 to (Array.length g.predicates)-1 do
    Format.pp_print_string fmt (g.predicates.(i).pname.name ^ ": ");
    Abstract1.print fmt state.(i);
    Format.pp_print_newline fmt ()
  done


(** Run the analysis. *)
let run_analysis
    (man: 'a Manager.t)
    (g: graph): 'a abstract_state =
  (* The abstract state. *)
  let state = make_initial_state man g in
  (* The predicates. *)
  let preds = g.predicates in
  (* Stability flag. *)
  let stable = ref false in

  (* Update the value at one point. *)
  let update (c: int): unit =
    let pre = List.map (evaluate_preconds man g state) preds.(c).clauses in
    let env = preds.(c).environment in
    let d = List.fold_left (fun d p -> Abstract1.join man d p) (Abstract1.bottom man env) pre in
    let d' = if preds.(c).widen then Abstract1.widening man state.(c) d else d in

    if not (Abstract1.is_eq man state.(c) d') then begin
      state.(c) <- d';
      stable := false
    end
  in

  (* Iterate. *)
  let rec iterate (c: int): unit =
    preds.(c).mark <- preds.(c).mark + 1;
    (* Waiting for additional values. *)
    if preds.(c).mark < List.length preds.(c).ancestors then begin
      (* node is widening point => continue (or wait until the end of times). *)
      if preds.(c).widen then
        List.iter iterate preds.(c).children
    (* All values are here. *)
    end else if preds.(c).head || preds.(c).mark = List.length preds.(c).ancestors then begin
      (* Compute the value at c. *)
      update c;
      (* Continue only if the point is not a widening point. *)
      if not preds.(c).widen then
        List.iter iterate preds.(c).children
    (* Already went through here, stop. *)
    end else ()
  in

  (* Iterate until stabilization. *)
  stable := false;
  while not !stable do
    stable := true;
    for i=0 to (Array.length preds)-1 do
      preds.(i).mark <- 0
    done;
    for i=0 to (Array.length preds)-1 do
      if preds.(i).head then iterate i
    done;
  done;
  state


(** Check the inferred abstract state against the negative clauses. *)
let check_negatives
    (man: 'a Manager.t)
    (g: graph)
    (state: 'a abstract_state): bool =
  List.for_all (fun c ->
    let d = evaluate_preconds man g state c in
    Abstract1.is_bottom man d
  ) g.negatives

