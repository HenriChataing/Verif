(** Syntax of the input language. *)

open Positions
open Expressions
open Expr
open Types
open Literal


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
let position_of_instruction (i:instruction): position =
  match i with
  | Assign (p,_,_) | While (p,_,_) | Declare (p,_,_)
  | If (p,_,_,_) | Break p | Continue p -> p

(** Boolean negation and other Expr.ts. *)
let bnot (e: Expr.t): Expr.t =
  let p = Expr.position e in
  Unary (p, "not", e)

let btrue: Expr.t =
  Prim (undefined_position, Bool true)

let bfalse: Expr.t =
  Prim (undefined_position, Bool false)

