(** Syntax of the input language. *)

(** Primitive types. *)
type ptype =
    TypeInt
  | TypeBool

(** Literals. *)
type literal =
    Int of int
  | Bool of bool

(** Expressions. *)
type expression =
    Var of string
  | Prim of literal
  | Binary of string * expression * expression
  | Unary of string * expression

(** Instructions and blocks. *)
type instruction =
    Assign of string * expression
  | While of expression * block
  | If of expression * block * block option
  | Break
  | Continue

and block = instruction list

