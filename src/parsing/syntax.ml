(** Syntax of the input language. *)

open Positions
open Expressions

(** Primitive types. *)
type ptype =
    TypeInt
  | TypeBool
  | TypeFloat


(** Printing. *)
let string_of_ptype (t: ptype): string =
  match t with
  | TypeInt -> "int" | TypeBool -> "bool"
  | TypeFloat -> "float"

(** Return the best type of a literal. *)
let typeof (l: literal): ptype =
  match l with
  | Int _ -> TypeInt | Float _ -> TypeFloat
  | Bool _ -> TypeBool

(** Return [true] iff [t0] is contained in [t1]. *)
let eqtype (t0: ptype) (t1: ptype) =
  match t0, t1 with
  | TypeInt, TypeInt -> true | TypeInt, TypeFloat -> true
  | TypeFloat, TypeFloat -> true
  | TypeBool, TypeBool -> true
  | _ -> false


(** Variables. *)
type var = {
  name: string;           (* The name of the variable. *)
  ptype: ptype            (* The type of declaration. *)
}

(** Expressions. *)
type expression =
    Var of position * var
  | Prim of position * literal
  | Binary of position * string * expression * expression
  | Unary of position * string * expression

(** Instructions and blocks. *)
type instruction =
    Assign of position * var * expression
  | Declare of position * var * expression option
  | While of position * expression * block
  | If of position * expression * block * block option
  | Break of position
  | Continue of position

and block = position * instruction list


(** Return the position of an expression. *)
let position_of_expression (e:expression): position =
  match e with
  | Var (p,_) | Prim (p,_)
  | Binary (p,_,_,_) | Unary (p,_,_) -> p

(** Return the position of an instruction. *)
let position_of_instruction (i:instruction): position =
  match i with
  | Assign (p,_,_) | While (p,_,_) | Declare (p,_,_)
  | If (p,_,_,_) | Break p | Continue p -> p

(** Boolean negation and other expressions. *)
let bnot (e: expression): expression =
  let p = position_of_expression e in
  Unary (p, "not", e)

let btrue: expression =
  Prim (undefined_position, Bool true)

let bfalse: expression =
  Prim (undefined_position, Bool false)


(** Printing. *)

let rec string_of_expression (e: expression): string =
  let parens_string_of_expression e =
    match e with
    | Binary _ | Unary _ -> "(" ^ string_of_expression e ^ ")"
    | _ -> string_of_expression e
  in
  match e with
  | Var (_,x) -> x.name
  | Prim (_,l) -> string_of_literal l
  | Binary (_,op,e0,e1) ->
      let pe0 = parens_string_of_expression e0
      and pe1 = parens_string_of_expression e1 in
      pe0 ^ " " ^ op ^ " " ^ pe1
  | Unary (_,op,e) ->
      let pe = parens_string_of_expression e in
      op ^ " " ^ pe

