(** Extract the Horn clauses from the SmtLib file. *)

open Smtlib
open Expressions
open Vars
open Types
open UnionFind
open Generics

module IntMap = Map.Make (struct
    type t = int
    let compare = (-)
  end)

(* List of smtlib operators, each given an arity and the used representation. *)
let operators = [
  "=", (2, "=="); "<>", (2, "<>");
  "<", (2, "<"); ">", (2, ">");
  "<=", (2, "<="); ">=", (2, ">=");
  "and", (2, "&&"); "or", (2, "||");
  "not", (1, "not");
  "+", (2, "+"); "-", (2, "-"); "*", (2, "*")
]


(** Definition of a clauses as returned by the parser. *)
type clause = (Expr.t list, var list, Expr.t list) gen_clause

type predicate = (var list, Expr.t list, clause) gen_predicate

(** Representation of the contents of a program writen using Horn clauses. *)
type script = (predicate, clause) gen_script



(** Convert a sort to a primitive type. *)
let rec typ_of_sort ?(arg: sort list = []) (s: sort): typ =
  let typ =
    match s with
    | Sort ({ symbol = "Int" }, []) -> TyInt
    | Sort ({ symbol = "Bool" }, []) -> TyBool
    | _ -> Errors.fatal [] "This sort is not a primitive type"
  in
  let arg = List.map typ_of_sort arg in
  if arg = [] then typ
  else TyArrow (arg, typ)

(** Convert a primitive type to a sort. *)
let rec sort_of_typ (t: typ): sort =
  match t with
  | TyInt -> Sort (make_id "Int", [])
  | TyBool -> Sort (make_id "Bool", [])
  | TyFloat -> Sort (make_id "Float", [])
  | TyArrow (_, typ) -> sort_of_typ typ


(** Remove the prefix universal quantifications and return the corresponding variables. *)
let rec strip_foralls (t: term): var list * term =
  match t with
  | Forall (_, xs, t) ->
      let xs = List.map (fun (s, t) ->
        { vid = 0; name = s; typ = typ_of_sort t; pos = Positions.undefined_position }) xs in
      let xs',t = strip_foralls t in
      xs @ xs', t
  | _ -> [], t

(** Remove the prefix existential quantifications and return the corresponding variables. *)
let rec strip_exists (t: term): var list * term =
  match t with
  | Exists (_, xs, t) ->
      let xs = List.map (fun (s, t) ->
        { vid = 0; name = s; typ = typ_of_sort t; pos = Positions.undefined_position }) xs in
      let xs',t = strip_exists t in
      xs @ xs', t
  | _ -> [], t


(** Use the term in conjunctive form. *)
let rec as_conjunction (t: term): term list =
  match t with
  | App (_, { symbol = "and" }, ts) ->
      List.concat (List.map as_conjunction ts)
  | _ -> [t]

(** Match the application of a predicate to its variables. *)
let as_predicate (ctx: context) (t: term): var * term list =
  match t with
  | App (pos, { symbol = c }, ts) ->
      from_context ~pos:pos ctx c, ts
  | _ -> Errors.fatal' (position_of_term t) "Term is not a Horn clause"


(** Convert a term to an expression. *)
let rec expr_of_term (ctx: context) (t: term): Expr.t =
  match t with
  | Prim (pos, Num n) -> Expr.Prim (pos, Primitive.Int n)
  | Ident (pos, { symbol = "true" }) ->
      Expr.Prim (pos, Primitive.Bool true)
  | Ident (pos, { symbol = "false" }) ->
      Expr.Prim (pos, Primitive.Bool false)

  | Ident (pos, { symbol = s }) ->
      Expr.Var (pos, from_context ~pos:pos ctx s)
  | App (pos, { symbol = op }, ts) ->
      let es = List.map (expr_of_term ctx) ts in
      begin try
        let (arity, repl) = List.assoc op operators in
        match op, arity, es with
        | "-", _, [e] -> Expr.Unary (pos, "-", e)
        | _, 1, [e] -> Expr.Unary (pos, repl, e)
        | _, 2, [e0;e1] -> Expr.Binary (pos, repl, e0, e1)
        | _ -> Errors.fatal [] "Malformed term"
      with Not_found ->
        Expr.Predicate (pos, from_context ~pos:pos ctx op, es)
      end
  | Attribute (_, t, _) -> expr_of_term ctx t
  | _ -> Errors.fatal [] "Not implemented"


(** Convert an expression to a term. *)
let rec term_of_expr (e: Expr.t): term =
  match e with
  | Expr.Prim (pos, Primitive.Int n) -> Prim (pos, Num n)
  | Expr.Prim (pos, Primitive.Bool true) ->
      Ident (pos, make_id "true")
  | Expr.Prim (pos, Primitive.Bool false) ->
      Ident (pos, make_id "false")

  | Expr.Var (pos, v) ->
      Ident (pos, make_id (vname v))

  | Expr.Unary (pos, op, e) ->
      begin try
        let op', _ = List.find (fun (_, (_, op')) -> op' = op) operators in
        App (pos, make_id op',
          [term_of_expr e])
      with Not_found ->
        Errors.fatal' pos ("Undefined operator '" ^ op ^ "'")
      end
  | Expr.Binary (pos, op, e0, e1) ->
      begin try
        let op', _ = List.find (fun (_, (_, op')) -> op' = op) operators in
        App (pos, make_id op',
          [term_of_expr e0; term_of_expr e1])
      with Not_found ->
        Errors.fatal' pos ("Undefined operator '" ^ op ^ "'")
      end
  | Expr.Predicate (pos, c, es) ->
      App (pos, make_id c.name,
        List.map term_of_expr es)

  | _ -> Errors.fatal [] "Not implemented"


(** Convert a clause to a term. *)
let term_of_clause (c: clause): term =
  let pre = match c.preconds with
    | [] -> Ident (c.cpos, make_id "true")
    | e::es ->
        List.fold_left (fun t e ->
          let te = term_of_expr e
          and p = Expr.position e in
          App (p, make_id "and", [t;te])
        ) (term_of_expr e) es
  in
  let impl =
    if c.negative then
      App (c.cpos, make_id "not", [pre])
    else begin
      let post =
        App (c.cpos,
          make_id c.cname.name,
          List.map term_of_expr c.arguments
        ) in
      App (c.cpos, make_id "=>", [pre;post])
    end
  in
  if c.variables = [] then
    impl
  else
    Forall (c.cpos,
      List.map (fun v -> vname v, sort_of_typ v.typ) c.variables,
      impl
    )


(** Extract the horn clauses of a smtlib program. *)
let extract_clauses (p: command list): script =
  (* Extract the predicate declarations and build the context. *)
  let context, commands, clauses =
    List.fold_left (fun (context, commands, clauses) com ->
      match com with
      (* Declaration of a predicate. *)
      | DeclareFun (pos, n, sts,st) ->
          let typ = typ_of_sort ~arg:sts st in
          let c = fresh_var ~pos:pos ~typ:typ ~name:n "C" in
          c::context, commands, clauses
      (* Clauses. *)
      | Assert _ ->
          context, commands, com::clauses
      (* Other. *)
      | _ ->
          context, com::commands, clauses
    ) ([], [], []) p
  in
  let pcontext = List.map (fun x -> x.name, x) context in
  (* Build the predicates. *)
  let predicates = Array.of_list (List.map (fun n ->
    { pname = n; arguments' = []; environment = []; clauses = [];
      mark = 0; head = false; widen = false;
      children = []; ancestors = []; fromloops = []; valid = true }
  ) (List.rev context)) in
  let negatives = ref [] in
  (* To add a dependency. *)
  let add_dep i j =
    predicates.(i).children <- Utils.insert j predicates.(i).children;
    predicates.(j).ancestors <- Utils.insert i predicates.(j).ancestors
  in
  (* To add a clause. *)
  let add_clause c =
    if not c.negative then begin
      let deps = List.concat (List.map Expr.predicates c.preconds) in
      List.iter (fun v -> add_dep v.vid c.cname.vid) deps;
      predicates.(c.cname.vid).clauses <- c::predicates.(c.cname.vid).clauses
    end else
      negatives := c::!negatives
  in
  (* Extract and insert the clauses. *)
  List.iter (fun c ->
    match c with
    | Assert (pos, t) ->
        (* Separate the quantifiers. *)
        let xs, t' = match t with
          | Forall _ -> strip_foralls t
          | App (pos, { symbol = "not" }, [t]) ->
              let xs, t' = strip_exists t in
              xs, App (pos, make_id "not", [t'])
          | _ -> [], t
        in
        let vs = List.map (fun x -> fresh_var ~pos:x.pos ~typ:x.typ "X") xs in
        (* Local context. *)
        let ctx = List.combine (List.map (fun x -> x.name) xs) vs @ pcontext in
        begin match t' with
        (* Basic form: P0 /\ .. /\ Pn => Q *)
        | App (pos, { symbol = "=>" }, [pre; post]) ->
            let es = List.map (expr_of_term ctx) (as_conjunction pre)
            and c, args = as_predicate ctx post in
            let args = List.map (expr_of_term ctx) args in
            add_clause { cname = c; cpos = pos;
              variables = vs; preconds = es;
              arguments = args; negative = false
            }
        (* Non explicit form of horn clauses: not (P0 /\ .. /\ Pn /\ not Q) *)
        | App (pos, { symbol = "not" }, [conj]) ->
            let cs = as_conjunction conj in
            (* Partition the goal and the preconditions. *)
            let goals, preconds = List.partition (function
              | App (_, { symbol = "not" }, [App (_, op, _)]) ->
                  List.exists (fun v -> v.name = op.symbol) context
              | _ -> false) cs in
            begin match goals with
            (* Exactly one goal => Positive and strict clauses. *)
            | [App (_, _, [t])] ->
                let es = List.map (expr_of_term ctx) preconds in
                let c,args = as_predicate ctx t in
                let args = List.map (expr_of_term ctx) args in
                add_clause { cname = c; cpos = pos;
                  variables = vs; preconds = es;
                  arguments = args; negative = false
                }
            (* No goal => Negative clauses. *)
            | [] ->
                let es = List.map (expr_of_term ctx) preconds in
                add_clause { cname = dummy_var; cpos = pos;
                  variables = vs; preconds = es;
                  arguments = []; negative = true
                }
            (* If more than one goal: not a horn clause. *)
            | _ -> Errors.fatal' pos "Unaccepted assertion: does not describe a Horn clause"
            end
      (* This case corresponds to clauses with no preconditions. *)
      | _ ->
          let c,args = as_predicate ctx t' in
          let args = List.map (expr_of_term ctx) args in
          add_clause { cname = c; cpos = pos;
            variables = vs; preconds = [];
            arguments = args; negative = false
          }
      end

    (* Other commands have already been removed. *)
    | _ -> assert false
  ) clauses;
  { context = context;
    predicates = predicates;
    commands = commands;
    negatives = !negatives;
    reducible = true }


(** Pretty print a horn clause (to something resembling the syntax of Prolog) *)
let string_of_clause (c: clause): string =
  let pre = match c.preconds with
    | [] -> ""
    | e::es -> Expr.to_string e ^ List.fold_left (fun s e -> s ^ ", " ^ Expr.to_string e) "" es
  in
  let vars = match c.arguments with
    | [] -> ""
    | a::args -> List.fold_left (fun s a -> s ^ ", " ^ Expr.to_string a) (Expr.to_string a) args
  in
  if c.negative then
    " :- " ^ pre
  else
    c.cname.name ^ "(" ^ vars ^ ") :- " ^ pre


(** Cleanup a clause by substituting variable equalities. *)
let substitute_equalities (c: clause): unit =
  (* Equivalence classes of variables. *)
  let classes = ref [] in
  let get_class rep x =
    let cx, cs = List.partition (fun (r,c) ->
      if rep then Some x = r else List.mem x c) !classes in
    classes := cs;
    match cx with
    | [] -> if rep then (Some x, []) else (None, [x])
    | cx::_ -> cx
  in
  let merge_classes rx x ry y =
    if x = y then []
    else begin
      let cx = get_class rx x
      and cy = get_class ry y in
      let mxy, eq = match cx, cy with
        | (None, cx), (ry, cy) -> (ry, cx @ cy), []
        | (rx, cx), (None, cy) -> (rx, cx @ cy), []
        | (Some rx, cx), (Some ry, cy) ->
            ((Some rx, cx @ cy),
             [Expr.Binary (rx.pos, "==", Expr.Var (rx.pos,rx), Expr.Var (ry.pos, ry))])
      in
      classes := mxy::!classes;
      eq
    end
  in
  (* Extract the goal predicate's arguments (they must be variables) *)
  let args = List.map (function
      Expr.Var (_,v) -> v
    | e -> Errors.fatal' (Expr.position e) "Expected variable in clause definition"
  ) c.arguments in

  (* Filter the preconditions for equalities between variables, and apply these
     equalities to the equivalence classes. *)
  let pre = List.fold_left (fun pre e ->
    match e with
    | Expr.Binary (_, "==", Expr.Var (_, v0), Expr.Var (_, v1)) ->
        let r0 = List.mem v0 args
        and r1 = List.mem v1 args in
        let eq = merge_classes r0 v0 r1 v1 in
        eq @ pre
    | _ -> e::pre
  ) [] c.preconds in
  (* Chose a representant by class, and build the corresponding substitution. *)
  let subs = List.fold_left (fun subs (r,c) ->
    let repr = match r with
      | None -> List.hd c
      | Some r -> r
    in
    List.fold_left (fun sub x -> (x, repr)::sub) subs c
  ) [] !classes in
  (* Apply the substitution to the remaining preconditions. *)
  let pre = List.map (Expr.rename subs) pre in
  (* Apply to the post condition. *)
  let post = List.map (Expr.rename subs) c.arguments in
  (* Build the resulting clause. *)
  c.preconds <- pre;
  c.arguments <- post


(** Identify a group of points cutting the loops, and mark them as widening points. *)
let identify_widening_points (g: script): unit =
  let size = Array.length g.predicates in
  (* Initialize the dominants. *)
  let dominants = Array.make size [] in
  for i=0 to size-1 do
    dominants.(i) <- [i]
  done;
  let stable = ref false in
  (* Compute the dominants. *)
  while not !stable do
    stable := true;
    for i=0 to size-1 do
      if g.predicates.(i).valid then begin
        let doms =
          Utils.insert i (
            Utils.intersect_list (
              List.map (fun p -> dominants.(p))
              g.predicates.(i).ancestors)) in
        if doms <> dominants.(i) then begin
          stable := false;
          dominants.(i) <- doms
        end
      end
    done
  done;

  (* Query the previously built relation. *)
  let dominant n d =
    List.mem d dominants.(n) in

  (* Compute the list of back edges. *)
  let backedges = ref [] in
  for i=0 to size-1 do
    if g.predicates.(i).valid then
      List.iter (fun d ->
        if dominant i d then begin
          g.predicates.(i).children <- Utils.delete d g.predicates.(i).children;
          g.predicates.(d).ancestors <- Utils.delete i g.predicates.(d).ancestors;
          backedges := (i, d)::!backedges
        end
      ) g.predicates.(i).children
  done;

  (* Check reducability. *)
  g.reducible <- true;
  let rec walk hist i =
    if g.predicates.(i).mark = 0 then begin
      g.predicates.(i).mark <- 1;
      List.iter (walk (i::hist)) g.predicates.(i).children
    end else if List.mem i hist then
      g.reducible <- false
  in

  for i=0 to size-1 do
    if g.predicates.(i).valid && g.predicates.(i).ancestors = [] then begin
      g.predicates.(i).head <- true;
      walk [] i
    end
  done;
  let explored = ref false in
  while not !explored do
    explored := true;
    for i=0 to size-1 do
      if g.predicates.(i).valid && g.predicates.(i).mark = 0 then begin
        explored := false;
        g.predicates.(i).head <- true;
        walk [] i
      end
    done
  done;


  (* Reinsert the backedges. *)
  List.iter (fun (n,d) ->
    g.predicates.(n).children <- Utils.insert d g.predicates.(n).children;
    g.predicates.(d).ancestors <- Utils.insert n g.predicates.(d).ancestors) !backedges;

  (* Reset the marks. *)
  for i=0 to size-1 do
    g.predicates.(i).mark <- 0
  done;

  if g.reducible then begin
    (* Mark the loop points. *)
    let rec looppoints (header: int) (i: int): unit =
      if header = i then ()
      else begin
        g.predicates.(i).fromloops <- Utils.insert header g.predicates.(i).fromloops;
        List.iter (looppoints header) g.predicates.(i).ancestors
      end in

    List.iter (fun (n, d) ->
      g.predicates.(d).widen <- true;
      g.predicates.(d).fromloops <- Utils.insert d g.predicates.(d).fromloops;
      looppoints d n
    ) !backedges
  end



(** Produce a DOT file with the dependency graph. *)
let make_dot (dot: string) (g: script): unit =
  let fout = open_out dot in
  let f = Format.formatter_of_out_channel fout in
  let open Format in
  pp_print_string f "digraph g {"; pp_print_newline f ();
  for i=0 to (Array.length g.predicates)-1 do
    if g.predicates.(i).valid then begin
      let color =
        if g.predicates.(i).widen && g.predicates.(i).head then "orange"
        else if g.predicates.(i).widen then "red"
        else if g.predicates.(i).head then "green"
        else "blue" in
      fprintf f "  %i [color=%s label=\"%s\" shape=box];" i color g.predicates.(i).pname.name;
      pp_print_newline f ()
    end
  done;
  for i=0 to (Array.length g.predicates)-1 do
    if g.predicates.(i).valid then
      List.iter (fun j ->
        let inter = Utils.intersect g.predicates.(i).fromloops g.predicates.(j).fromloops in
        if inter <> [] then
          fprintf f "  %i -> %i [color=red] ;" i j
        else
          fprintf f "  %i -> %i;" i j;
        pp_print_newline f ()
      ) g.predicates.(i).children
  done;
  pp_print_string f "}";
  pp_print_flush f ()


(** Rename the arguments of the goal predicate to match the format X{cid}_{vid}. *)
let standardise_arguments (p: script): unit =
  for i=0 to (Array.length p.predicates)-1 do
    let cname = p.predicates.(i).pname in
    let typargs =
      match cname.typ with
      | TyArrow (ts, t) -> ts
      | _ -> []
    in

    (* Unified arguments. *)
    let _,uargs = List.fold_left (fun (i, uargs) ty ->
      let v = {
        name = "A" ^ string_of_int cname.vid;
        pos = cname.pos; typ = ty; vid = i } in
      i+1, v::uargs
    ) (0, []) typargs in
    let uargs = List.rev uargs in
    let vargs = List.map (fun v -> Expr.Var (v.pos, v)) uargs in

    p.predicates.(i).arguments' <- uargs;

    (* Replace the old by the new. *)
    List.iter (fun c ->
      let subs, eqs = List.fold_left (fun (subs, eqs) (v, e) ->
        match e with
        | Expr.Var (_, v') ->
            begin try
              let _ = List.assoc v' subs in
              subs, Expr.Binary (v.pos, "==", Expr.Var (v.pos, v), e)::eqs
            with Not_found ->
              (v', v)::subs, eqs
            end
        | _ -> subs, Expr.Binary (v.pos, "==", Expr.Var (v.pos, v), e)::eqs
      ) ([], []) (List.combine uargs c.arguments) in
      let preconds = List.map (Expr.rename subs) (c.preconds @ eqs) in
      let variables = List.filter (fun v ->
        List.for_all (fun (v', _) -> v <> v') subs
      ) c.variables @ uargs in

      let diff = List.length variables - List.length c.variables in
      Logger.log ~mode:"simpl" ("StdArg:" ^ c.cname.name ^": +" ^ string_of_int diff ^ "\n");
      Logger.flush ();

      c.arguments <- vargs;
      c.variables <- variables;
      c.preconds <- preconds
    ) p.predicates.(i).clauses
  done


(** Inline the clauses that are called called only once. *)
let inline_clauses (g: script): unit =
  let preds = g.predicates in

  (* Inline a predicate in a clause. *)
  let inline (pc: clause) (c: clause) =
    let pid = pc.cname.vid in
    (* Duplicate a predicate. *)
    let duplicate (es: Expr.t list): Expr.t list * var list =
      let subs = List.combine preds.(pid).arguments' es in
      let vars = Utils.difference pc.variables preds.(pid).arguments' in
      let pvars = List.map (fun v -> fresh_var ~pos:v.pos ~typ:v.typ v.name) vars in
      let rename = List.combine vars pvars in
      let es = List.map (fun e -> Expr.subs subs (Expr.rename rename e)) pc.preconds in
      es, pvars
    in
    (* Repeat for all predicates in the preconditions. *)
    let pre, pvars = List.fold_left (fun (pre, pvars) e ->
      match e with
      | Expr.Predicate (_, p', es) when p' = pc.cname ->
          let es', pvars' = duplicate es in
          es' @ pre, pvars' @ pvars
      | _ -> e::pre, pvars
    ) ([],[]) c.preconds in
    c.preconds <- pre;
    c.variables <- pvars @ c.variables;
    (* Modify graph information. *)
    if not c.negative then begin
      if preds.(pid).head then
        preds.(c.cname.vid).head <- true;
      preds.(pid).head <- false;  (* Ignore this predicate during analysis. *)
      preds.(c.cname.vid).ancestors <- Utils.delete pid preds.(c.cname.vid).ancestors;
      preds.(c.cname.vid).ancestors <- Utils.union preds.(c.cname.vid).ancestors preds.(pid).ancestors;
      List.iter (fun i ->
        preds.(i).children <- Utils.insert c.cname.vid (Utils.delete pid preds.(i).children)
      ) preds.(pid).ancestors
    end
  in

  (* Start a graph walk, inlining all possible clauses. *)
  let rec walk (i: int): unit =
    if preds.(i).mark = 0 then begin
      preds.(i).mark <- 1;
      match preds.(i).children, preds.(i).clauses with
      | [j], [c] ->
          Logger.log ~mode:"simpl" ("Inline " ^ preds.(i).pname.name ^ " -> " ^ preds.(j).pname.name); Logger.newline ~mode:"simpl" ();
          List.iter (inline c) preds.(j).clauses;
          List.iter (inline c) g.negatives;

          preds.(i).valid <- false;
          walk j
      | cs, _ -> List.iter walk cs
    end
  in

  for i=0 to (Array.length g.predicates)-1 do
    if g.predicates.(i).head then
      walk i
  done


(** Use of an argument. *)
type arguse =
    Never
  | Always
  | Depends of (int * int) list  (* j-th argument of the i-th clause. *)

let join (u: arguse) (u': arguse): arguse =
  match u,u' with
  | Always, _ -> Always | _, Always -> Always
  | Never, _ -> u' | _, Never -> u
  | Depends u, Depends u' -> Depends (u @ u')


(** On a global scale, purge the useless clause arguments.
    This is done by using abstract interpretation with a relational domain
    that keeps for each variable the contrained state (=the dimension of the abstract value)
    and the equalities between variables. *)
let minimize_clause_arguments (p: script): unit =
  (* Make fresh union variables for the arguments of a predicate. *)
  let make_uvars xs =
    Array.of_list (List.map (fun x -> make_uvar x Never) xs) in

  (* The abstract state. *)
  let state = Array.map (fun p ->
    if p.valid then make_uvars p.arguments'
    else [||]) p.predicates in

  (* Update the usage of an argument in a local state. *)
  let update x u lstate =
    UnionFind.update lstate.(x.vid) (join u) in

  (* Reset a local state. *)
  let reset lstate =
    for i=0 to (Array.length lstate)-1 do
      lstate.(i).parent <- Utils.Left Never
    done in

  (* Merge two equivalence classes == insert an equality. *)
  let equals x y lstate =
    UnionFind.union ~join:join lstate.(x.vid) lstate.(y.vid) in

  (* Retrieve the usage of an argument. *)
  let usage c i =
    UnionFind.value state.(c.vid).(i) in

  (* Check whether two arguments are in the same class. *)
  let same c i j =
    UnionFind.find state.(c.vid).(i) == UnionFind.find state.(c.vid).(j) in

  (* Evaluate an expression. *)
  let rec evaluate lstate e =
    match e with
    | Expr.Var (_, v) when isarg v -> update v Always lstate
    | Expr.Predicate (_, c, es) ->
        (* Join with the equivalence of predicate c. *)
        Utils.iteri (fun i e ->
          if usage c i = Always then evaluate lstate e;
          match e with
          | Expr.Var (_, v) when isarg v ->
              Utils.iteri (fun j e' ->
                if j > i then begin
                  match e' with
                  | Expr.Var (_, v') when isarg v' && same c i j ->
                      equals v v' lstate
                  | _ -> ()
                end
              ) es
          | _ -> ()
        ) es
    | Expr.Binary (_, _, e0, e1) -> evaluate lstate e0; evaluate lstate e1
    | Expr.Unary (_, _, e) -> evaluate lstate e
    | _ -> () in

  (* Evaluate a list of preconditions. *)
  let evaluate_preconds lstate es =
    List.iter (fun e ->
      match e with
      | Expr.Binary (_, "==", Expr.Var (_,v0), Expr.Var (_,v1)) ->
          equals v0 v1 lstate
      | _ -> evaluate lstate e
    ) es in

  (* Display a partition. *)
  let print_lstate lstate =
    Logger.execute ~mode:"simpl-debug" (fun _ ->
      (* Build the equivalence classes. *)
      let equivs = ref IntMap.empty in
      for j=0 to (Array.length lstate)-1 do
        let c = (UnionFind.find lstate.(j)).var.vid in
        let cur = try IntMap.find c !equivs with Not_found -> [] in
        equivs := IntMap.add c (lstate.(j).var::cur) !equivs
      done;

      IntMap.iter (fun c vs ->
        Logger.log "[ "; List.iter (fun uv -> Logger.log (vname uv ^ " ")) vs;
        if UnionFind.value lstate.(c) = Always then Logger.log "] = [ Always ]\n"
        else Logger.log "] = [ Never ]\n"
      ) !equivs
    ) in

  (* Join the partitions of two local states (as coming from different clauses). *)
  let join_lstates lstate0 lstate1 dest =
    let size = Array.length lstate0 in
    (* Reset the destination. *)
    for i=0 to size-1 do
      dest.(i).parent <- Utils.Left (join (UnionFind.value lstate0.(i)) (UnionFind.value lstate1.(i)))
    done;
    (* Derive the equivalence classes. *)
    for i=0 to size-2 do
      for j=i+1 to size-1 do
        (* i and j are in the same class iff they are in both states. *)
        if UnionFind.find lstate0.(i) == UnionFind.find lstate0.(j) &&
           UnionFind.find lstate1.(i) == UnionFind.find lstate1.(j) then
          UnionFind.union dest.(i) dest.(j)
      done
    done in

  (* Update the value of a single predicate. *)
  let update_predicate (c: int): unit =
    (* Make usable local states. *)
    let lstate0 = ref (make_uvars p.predicates.(c).arguments') in
    match p.predicates.(c).clauses with
    | [] -> ()
    | [cl] -> begin
        evaluate_preconds !lstate0 cl.preconds;
        state.(c) <- !lstate0
      end
    | cl::cls -> begin
        (* Make more local states. Can't use state.(c) because of self recursing
           predicates. *)
        let lstate1 = ref (make_uvars p.predicates.(c).arguments')
        and lstate2 = ref (make_uvars p.predicates.(c).arguments') in
        (* Evaluate each clause, and join the values.
           For each iteration :
            - lstate0 serves to compute the value of the clause
            - lstate1 contains the join value
            - lstate2 serves to compute the join value. *)
        evaluate_preconds !lstate1 cl.preconds;
        List.iter (fun cl ->
          reset !lstate0;
          evaluate_preconds !lstate0 cl.preconds;
          join_lstates !lstate0 !lstate1 !lstate2;  (* lstate2 is reset by the function call. *)
          let tmp = !lstate2 in
          lstate2 := !lstate1;
          lstate1 := tmp
        ) cls;
        (* At this point, lstate1 contains the final value. *)
        state.(c) <- !lstate1
      end
  in

  (* Compare two states. *)
  let compare lstate0 lstate1 =
    let eq = ref true in
    for i=0 to (Array.length lstate0)-1 do
      if UnionFind.value lstate0.(i) <> UnionFind.value lstate1.(i) then eq := false
    done in

  (* Run the analysis. *)
  let stable = ref false in
  while not !stable do
    stable := true;
    for i=0 to (Array.length p.predicates)-1 do
      p.predicates.(i).mark <- if p.predicates.(i).head then -1 else 0;
    done;
    for i=0 to (Array.length p.predicates)-1 do
      if p.predicates.(i).head then Generics.iterate p.predicates update_predicate i
    done
  done;

  (* Display the results. *)
  Logger.execute ~mode:"simpl-debug" (fun _ ->
    for i=0 to (Array.length p.predicates)-1 do
      if p.predicates.(i).valid then begin
        Logger.log ("Predicate " ^ vname p.predicates.(i).pname ^ ":\n");
        print_lstate state.(i)
      end
    done;
    Logger.flush ()
  );

  (* At this point, all the abstract values, which correspond to equivalence
    classes of the predicate arguments, have been computed. To determine which
    variable should be kept :
     - if the class representant is unused, remove the variable
     - else if is the representant, keep the variable
     - else remove
    Equalities are generated where predicates are used to
    make up for the missing arguments and constraints. *)

  let keep (c: int) (i: int): bool =
    if UnionFind.value state.(c).(i) = Never then false
    else UnionFind.find state.(c).(i) == state.(c).(i) in

  (* Apply the modifications to an expression. *)
  let rec modify (c: int) (e: Expr.t): Expr.t * Expr.t list =
    match e with
    | Expr.Var (pos,v) when isarg v ->
        let v' = UnionFind.find state.(c).(v.vid) in
        Expr.Var (pos,v'.var), []
    | Expr.Binary (pos, op, e0, e1) ->
        let e0', eqs0 = modify c e0
        and e1', eqs1 = modify c e1 in
        Expr.Binary (pos, op, e0', e1'), eqs0 @ eqs1
    | Expr.Unary (pos, op, e) ->
        let e', eqs = modify c e in
        Expr.Unary (pos, op, e'), eqs
    | Expr.Predicate (pos, c', es) ->
        (* In the case of a predicate application :
            - the arguments are filtered to remove duplicate and unused variables.
            - Equalities are generated matching the duplicates. *)
        let _, es', eqs = List.fold_left (fun (i, es', eqs) e ->
          let ri = UnionFind.find state.(c'.vid).(i) in
          (* Check for inherited constraints. *)
          let inherited =
            if ri != state.(c'.vid).(i) then
              [Expr.Binary (pos, "==", e, List.nth es ri.var.vid)]
            else
              []
          in
          (* Delete or not the argument. *)
          if keep c'.vid i then
            let e', eqs' = modify c e in
            i+1, e'::es', eqs' @ inherited @ eqs
          else
            i+1, es', inherited @ eqs
        ) (0, [], []) es in
        Expr.Predicate (pos, c', List.rev es'), eqs
    | _ -> e, [] in

  (* Apply the modifications to a list of preconditions. *)
  let rec modify_preconds (c: int) (es: Expr.t list): Expr.t list =
    List.fold_left (fun pre e ->
      match e with
      | Expr.Binary (pos, "==", Expr.Var (_,v0), Expr.Var (_,v1)) when isarg v0 && isarg v1 ->
          let r0 = UnionFind.find state.(c).(v0.vid)
          and r1 = UnionFind.find state.(c).(v1.vid) in
          if r0 == r1 then pre
          else
            Expr.Binary (pos, "==",
              Expr.Var (r0.var.pos, r0.var),
              Expr.Var (r1.var.pos, r1.var)
            )::pre
      | e ->
          let e', eqs = modify c e in
          let eqs' = modify_preconds c eqs in
          e'::eqs' @ pre
    ) [] es in

  (* Update the type of each predicate. *)
  List.iter (fun c ->
    if p.predicates.(c.vid).valid then
      let arg, typ = match c.typ with
        | TyArrow (arg, typ) -> arg,typ
        | _ -> [], c.typ
      in
      match Utils.filteri (fun i _ -> keep c.vid i) arg with
      | [] -> c.typ <- typ
      | arg -> c.typ <- TyArrow (arg, typ)
  ) p.context;

  (* Update the definition of the predicates. *)
  for i=0 to (Array.length p.predicates)-1 do
    if p.predicates.(i).valid then begin
      let old = List.length p.predicates.(i).arguments' in
      p.predicates.(i).arguments' <- Utils.filteri (fun j _ -> keep i j) p.predicates.(i).arguments';

      let diff = old - List.length p.predicates.(i).arguments' in
      Logger.log ~mode:"simpl" ("MinArg:" ^ p.predicates.(i).pname.name ^ ": " ^
        string_of_int (-diff) ^ " " ^ " (" ^ string_of_int (100 * diff / old) ^ "%)\n");
      Logger.flush ();

      List.iter (fun c ->
        c.preconds <- modify_preconds i c.preconds;
        c.arguments <- Utils.filteri (fun j _ -> keep i j) c.arguments
      ) p.predicates.(i).clauses
    end
  done;

  (* Update the negative clauses. *)
  List.iter (fun c ->
    c.preconds <- modify_preconds 0 c.preconds
  ) p.negatives



(** Remove the unconstrained variables existentially declared in a clause. *)
let minimize_clause_variables (c: clause): unit =
  let used = List.concat (List.map Expr.freevar (c.preconds @ c.arguments)) in
  let rem = List.filter (fun v -> List.mem v used) c.variables in

  let name = if c.negative then "Neg" ^ string_of_int c.cname.vid else c.cname.name in
  let diff = List.length rem - List.length c.variables in
  Logger.log ~lvl:2 ("MinVar:" ^ name ^ ": " ^ string_of_int diff ^ "\n");
  Logger.flush ();

  c.variables <- rem


(** Try reducing the number of arguments and variables. *)
let simplify_clauses ?(inline: bool = true) (p: script): unit =
  Logger.log ~mode:"simpl" "### Clause minimalization ###"; Logger.newline ~mode:"simpl" ();
  standardise_arguments p;
  if inline then inline_clauses p;
  for i=0 to (Array.length p.predicates)-1 do
    List.iter (fun c ->
      substitute_equalities c
    ) p.predicates.(i).clauses;
  done;
  List.iter substitute_equalities p.negatives;
  minimize_clause_arguments p;
  for i=0 to (Array.length p.predicates)-1 do
    List.iter minimize_clause_variables p.predicates.(i).clauses
  done;
  List.iter minimize_clause_variables p.negatives

(** Rebuild a simplified smt program. *)
let commands_of_script (p: script): command list =
  let decl, clauses = List.fold_left (fun (decls, cs) pred ->
    if not pred.valid then
      decls, cs
    else begin
      let arg, typ = match pred.pname.typ with
        | TyArrow (arg, typ) -> arg, typ
        | _ -> [], pred.pname.typ
      in
      let arg = List.map sort_of_typ arg
      and typ = sort_of_typ typ in
      let dcl = DeclareFun (pred.pname.pos, pred.pname.name, arg, typ) in
      dcl::decls, pred.clauses @ cs
    end) ([], p.negatives) (Array.to_list p.predicates) in
  let asserts = List.map (fun c ->
    Assert (c.cpos, term_of_clause c)
  ) clauses in
  List.sort (fun c0 c1 ->
    let p0 = position_of_command c0
    and p1 = position_of_command c1 in
    compare (Positions.start_of_position p0) (Positions.start_of_position p1)
  ) (decl @ asserts @ p.commands)

