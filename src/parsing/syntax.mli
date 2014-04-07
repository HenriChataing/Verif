(** Syntax of the input language. *)

open Positions

(** Primitive types. *)
type ptype =
    TypeInt
  | TypeBool
  | TypeFloat

(** Literals. *)
type literal =
    Int of int
  | Bool of bool

(** Expressions. *)
type expression =
    Var of position * string
  | Prim of position * literal
  | Binary of position * string * expression * expression
  | Unary of position * string * expression

(** Instructions and blocks. *)
type instruction =
    Assign of position * string * expression
  | Declare of position * ptype * string * expression option
  | While of position * expression * block
  | If of position * expression * block * block option
  | Break of position
  | Continue of position

and block = position * instruction list


(** Return the position of an expression. *)
val position_of_expression: expression -> position

(** Return the position of an instruction. *)
val position_of_instruction: instruction -> position

(** Boolean negation and other expressions. *)
val bnot: expression -> expression
val btrue: expression
val bfalse: expression

(** Printing. *)
val string_of_literal: literal -> string
val string_of_expression: expression -> string

