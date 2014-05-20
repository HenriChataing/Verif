(** Extract the Horn clauses from the SmtLib program. *)

open Smtlib
open Vars
open Expressions
open Types


(* Definition of clauses as returned by the parser. *)
type clause = (Expr.t list, var list, Expr.t list) Generics.gen_clause
type predicate = (var list, Expr.t list, clause) Generics.gen_predicate
type script = (predicate, clause) Generics.gen_script

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



(** Extract the horn clauses of a smtlib program. *)
val extract_clauses: command list -> script

(** Identify a subset of predicates cutting all loops. *)
val identify_widening_points: script -> unit

(** Produce a DOT file with the dependency graph of the predicates. *)
val make_dot: string -> script -> unit

(** Print a horn clause. *)
val string_of_clause: clause -> string


(** Simplify the clauses by removing irrelevant arguments. *)
val simplify_clauses: script -> unit

(** Rebuild a simplified smt program. *)
val commands_of_script: script -> command list

