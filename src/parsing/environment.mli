(** Variables and generation of unique variable names. This module also
    includes means of constructing scopes. *)


(** Check for the existance of a variable in the current environment. *)
val find_var: Positions.position -> string -> Syntax.var

(** Create a new variable. *)
val create_var: string -> Syntax.ptype -> Syntax.var

(** Perform some operation in a temporary scope. *)
val in_scope: (unit -> 'a) -> 'a

(** Return the list of all typed variables. *)
val typed_variables: unit -> (string * Syntax.ptype) list

(** Type checking and scope analysis of a program. *)
val typecheck: Syntax.block -> Syntax.block

