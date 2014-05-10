(** Primitives include integers, floating point numbers and booleans. *)

open Types

(** Type of primitives. *)
type t =
    Int of int
  | Bool of bool
  | Float of float

(** String conversion. *)
val to_string: t -> string

(** Return the smallest type of a primitive. *)
val typeof: t -> typ

(** Addition. Integers are automatically converted to floating point numbers
    when needed. *)
val add: t -> t -> t

(** Multiplication. Integers are automatically converted to floating point numbers
    when needed. *)
val mul: t -> t -> t

(** Negation. *)
val neg: t -> t

(** Is the primitive nul. *)
val eq0: t -> bool

(** Comparison to 0. *)
val lt0: t -> bool

(** Conversion to Aron coefficients. *)
val to_coeff: t -> Apron.Coeff.t

