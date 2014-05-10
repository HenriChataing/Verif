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
  mutable preconds: ((Tcons1.t, var * Texpr1.t list) either Bexpr.t) list;
  negative: bool
} 

type node = {
  pname: var;                  (* The name of the predicate. *)
  parguments: Environment.t;   (* The arguments of the predicate. *)
  clauses: clause list;        (* The list of clauses whose goal is the predicate. *)
  mutable mark: bool;          (* A boolean marker. *)
  mutable widen: bool          (* Identify widening points. *)
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
  : (Tcons1.t, var * Texpr1.t list) either Bexpr.t =
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
      Atom (Right (c, List.map (texpr_of_expr env) es))
  | _ -> Errors.fatal' (Expr.position e) "This expression is not boolean"


(** Return the list of predicates appearing in a boolean expression. *)
let rec predicates_of_bexpr (b: (Tcons1.t, var * Texpr1.t list) either Bexpr.t): var list =
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
    parguments = Environment.make [||] [||];
    clauses = []; mark = false; widen = false
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
      parguments = env;
      clauses = cs; mark = false; widen = false
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
  (* Create a graph with both incoming and outgoing edges. *)
  let g' = Array.make (Array.length g.predicates) ([], []) in
  let add i j =
    let ein, eout = g'.(i) in
    g'.(i) <- (ein, Utils.insert j eout);
    let ein, eout = g'.(j) in
    g'.(j) <- (Utils.insert i ein, eout)
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
    if not g.predicates.(i).mark then begin
      g.predicates.(i).mark <- true;
      List.iter (walk (i::hist)) (snd g'.(i))
    end else if List.mem i hist then
      g.predicates.(i).widen <- true
  in
  (* Launch the exploration on all nodes with no incoming edges. *)
  for i=0 to (Array.length g')-1 do
    if fst g'.(i) = [] then walk [] i
  done;
  (* If it isn't enough, pick up other nodes until all nodes have been visited. *)
  let finished = ref false in
  while not !finished do
    finished := true;
    for i=0 to (Array.length g')-1 do
      if not g.predicates.(i).mark then begin
        finished := false;
        walk [] i
      end
    done
  done;
  (* Reinitialize the markers. *)
  for i=0 to (Array.length g')-1 do
    g.predicates.(i).mark <- false
  done;
  (* Write the dot file, if needed. *)
  if dot <> "" then begin
    let fout = open_out dot in
    let f = Format.formatter_of_out_channel fout in
    let open Format in
    pp_print_string f "digraph g {"; pp_print_newline f ();
    for i=0 to (Array.length g')-1 do
      if g.predicates.(i).widen then
        fprintf f "  %i [color=red label=\"%s\" shape=box];" i g.predicates.(i).pname.name
      else if (fst g'.(i)) = [] then
        fprintf f "  %i [color=green label=\"%s\" shape=box];" i g.predicates.(i).pname.name
      else
        fprintf f "  %i [color=blue label=\"%s\" shape=box];" i g.predicates.(i).pname.name;
      pp_print_newline f ()
    done;
    for i=0 to (Array.length g')-1 do
      List.iter (fun j ->
        fprintf f "  %i -> %i;" i j;
        pp_print_newline f ()
      ) (snd g'.(i))
    done;
    pp_print_string f "}";
    pp_print_flush f ();
  end

