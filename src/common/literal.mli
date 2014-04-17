(** Literals include integers, floating point numbers and booleans. *)

open Types

(** Type of literals. *)
type t =
    Int of int
  | Bool of bool
  | Float of float

(** String conversion. *)
val to_string: t -> string

(** Return the smallest type of a literal. *)
val typeof: t -> ptype

(** Addition. Integers are automatically converted to floating point numbers
    when needed. *)
val add: t -> t -> t

(** Multiplication. Integers are automatically converted to floating point numbers
    when needed. *)
val mul: t -> t -> t

(** Negation. *)
val neg: t -> t

(** Is the literal nul. *)
val eq0: t -> bool

(** Comparison to 0. *)
val lt0: t -> bool

