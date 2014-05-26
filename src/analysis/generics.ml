(** Build and analyze the dependency graph of the horn clauses. *)

open Apron
open Expressions
open Expr
open Bexpr
open Utils
open Smtlib
open Vars
open Types
open Positions


(** Parametric definition of clauses. *)
type ('a, 'v, 'p) gen_clause = {
  cname: var;               (* The clause's predicate. *)
  cpos: position;           (* The position of the declaration. *)
  mutable variables: 'v;    (* List of universally quantified variables. *)
  mutable preconds: 'p;     (* Clause preconditions. *)
  mutable arguments: 'a;    (* Arguments of the clause. *)
  negative: bool            (* Whether the goal is negative or positive or strict.
                               If it is negative, the fields cname and arguments are ignored. *)
}

(** Parametric definition of predicates. *)
type ('a, 'e, 'c) gen_predicate = {
  mutable pname: var;          (* The name of the predicate. *)
  mutable arguments': 'a;      (* The arguments of the predicate. *)
  mutable environment: 'e;     (* The associated Apron environment. *)
  mutable clauses: 'c list;    (* The list of clauses whose goal is the predicate. *)

  mutable mark: int;           (* A boolean marker. *)
  mutable widen: bool;         (* Identify widening points. *)
  mutable head: bool;          (* Identify the starting points of the analysis. *)

  mutable children: int list;  (* The list of predicates that depends upon this node. *)
  mutable ancestors: int list; (* The number of dependencies. *)
  mutable fromloops: int list; (* List the loops this point is part of. *)

  mutable valid: bool          (* When the predicate has been inlined, it shouldn't be accessed. *)
}

(** Group the predicates definitions in a dependency graph. *)
type ('p, 'c) gen_script = {
  context: var list;           (* List of predicates. *)
  predicates: 'p array;        (* Definition of each predicate. *)
  negatives: 'c list;          (* Remaining, negative clauses. *)
  commands: command list       (* Remaining commands. *)
}


(** Several functions querying the control graph. *)
let fromloop (script: ('p,'c) gen_script) (loop: int) (c: int) =
  List.mem loop script.predicates.(c).fromloops


(** The iteration function of the static analysis. *)
let rec iterate (predicates: 'p array) (update: int -> unit) (c: int): unit =
  predicates.(c).mark <- predicates.(c).mark + 1;
  (* Waiting for additional values. *)
  if predicates.(c).mark < List.length predicates.(c).ancestors then begin
    (* node is widening point => continue. *)
    if predicates.(c).widen then begin
      update c;
      List.iter (iterate predicates update) predicates.(c).children
    end
  (* All values are here. *)
  end else if predicates.(c).head || predicates.(c).mark = List.length predicates.(c).ancestors then begin
    (* Compute the value at c. *)
    update c;
    (* Continue only if the point is not a widening point. *)
    if not predicates.(c).widen then
      List.iter (iterate predicates update) predicates.(c).children
  (* Already went through here, stop. *)
  end else ()

