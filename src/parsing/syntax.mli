(** Syntax of the input language. *)

open Positions
open Expressions


(** Instructions and blocks. *)
type instruction =
    Assign of position * var * Expr.t
  | Declare of position * var * Expr.t option
  | While of position * Expr.t * block
  | If of position * Expr.t * block * block option
  | Break of position
  | Continue of position

and block = position * instruction list


(** Return the position of an instruction. *)
val position_of_instruction: instruction -> position

(** Boolean negation and other expressions. *)
val bnot: Expr.t -> Expr.t
val btrue: Expr.t
val bfalse: Expr.t


