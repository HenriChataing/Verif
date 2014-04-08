(** Variables and generation of unique variable names. This module also
    includes means of constructing scopes. *)


(** Check for the existance of a variable in the current environment. *)
val find_var: Positions.position -> string -> string

(** Create a new variable. *)
val create_var: string -> Syntax.ptype -> unit

(** Open a new scope. *)
val open_scope: unit -> unit

(** Close the top scope. *)
val close_scope: unit -> unit

(** Perform some operation in a temporary scope. *)
val in_scope: (unit -> 'a) -> 'a

(** Return the list of all typed variables. *)
val typed_variables: unit -> (string * Syntax.ptype) list

