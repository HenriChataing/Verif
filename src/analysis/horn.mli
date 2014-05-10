(** Extract the Horn clauses from the SmtLib program. *)

open Smtlib
open Vars
open Expressions
open Types

(** Definition of a horn clause. *)
type clause = {
  cname: var;               (* The clause's name. *)
  cpos: Positions.position; (* The position of the declaration. *)
  mutable variables: var list;      (* List of universally quantified variables. *)
  mutable preconds: Expr.t list;    (* Clause preconditions. *)
  mutable arguments: Expr.t list;   (* Arguments of the clause. *)
  negative: bool            (* Whether the goal is negative or positive or strict.
                               If it is negative, the fields cname and arguments are ignored. *)
}



(** Convert a sort to a primitive type. *)
val typ_of_sort: ?arg: sort list -> sort -> typ

(** Convert a primitive type to a sort. *)
val sort_of_typ: typ -> sort


(** Remove the prefix universal quantifications and return the corresponding variables. *)
val strip_foralls: term -> var list * term

(** Remove the prefix existential quantifications and return the corresponding variables. *)
val strip_exists: term -> var list * term

(** Use the term in conjunctive form. *)
val as_conjunction: term -> term list

(** Match the application of a predicate to its variables. *)
val as_predicate: context -> term -> var * term list

(** Convert a term to an expression. *)
val expr_of_term: context -> term -> Expr.t

(** Convert an expression to a term. *)
val term_of_expr: Expr.t -> term

(** Convert a clause to a term. *)
val term_of_clause: clause -> term


(** Representation of the contents of a program writen using Horn clauses. *)
type script = {
  context: var list;
  clauses: clause list;
  commands: command list
}
 
(** Extract the horn clauses of a smtlib program. *)
val extract_clauses: command list -> script

(** Print a horn clause. *)
val string_of_clause: clause -> string


(** Simplify the clauses by removing irrelevant arguments. *)
val simplify_clauses: script -> unit

(** Rebuild a simplified smt program. *)
val commands_of_script: script -> command list

