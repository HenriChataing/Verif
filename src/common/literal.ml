(** Literals include integers, floating point numbers and booleans. *)

open Types

(** Type of literals. *)
type t =
    Int of int
  | Bool of bool
  | Float of float


(** String conversion. *)
let to_string (l: t): string =
  match l with
  | Int n -> string_of_int n
  | Float f -> string_of_float f
  | Bool true -> "true"
  | Bool false -> "false"

(** Return the smallest type of a literal. *)
let typeof (l: t): ptype =
  match l with
  | Int _ -> TypeInt | Float _ -> TypeFloat
  | Bool _ -> TypeBool

(** Addition. Integers are automatically converted to floating point numbers
    when needed. *)
let add (l0: t) (l1: t): t =
  match l0, l1 with
  | Int n0, Int n1 -> Int (n0 + n1)
  | Float f0, Float f1 -> Float (f0 +. f1)
  | Int n0, Float f1 -> Float (float n0 +. f1)
  | Float f0, Int n1 -> Float (f0 +. float n1)
  | _ -> Errors.fatal [Lexing.dummy_pos] "Bad operation on literals"

(** Multiplication. Integers are automatically converted to floating point numbers
    when needed. *)
let mul (l0: t) (l1: t): t =
  match l0, l1 with
  | Int n0, Int n1 -> Int (n0 * n1)
  | Float f0, Float f1 -> Float (f0 *. f1)
  | Int n0, Float f1 -> Float (float n0 *. f1)
  | Float f0, Int n1 -> Float (f0 *. float n1)
  | _ -> Errors.fatal [Lexing.dummy_pos] "Bad operation on literals"

(** Negation. *)
let neg (l: t): t =
  match l with
  | Int n -> Int (-n) | Float f -> Float (-.f)
  | _ -> Errors.fatal [Lexing.dummy_pos] "Bad operation on literals" 

(** Is the literal nul. *)
let eq0 (l: t): bool =
  l = Int 0 || l = Float 0.

(** Comparison to 0. *)
let lt0 (l: t): bool =
  match l with
  | Int n -> n < 0 | Float f -> f < 0.
  | _ -> Errors.fatal [Lexing.dummy_pos] "Bad operation on literals"

