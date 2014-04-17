(** Contains the definition of several kinds of expressions. *)

open Positions
open Literal
open Types

(** Variables. *)
type var = {
  name: string;           (* The name of the variable. *)
  ptype: ptype            (* The type of declaration. *)
}


(** Basic expressions. *)
module Expr = struct
  (** Type of expressions. *)
  type t =
     Var of position * var
  | Prim of position * Literal.t
  | Binary of position * string * t * t
  | Unary of position * string * t


  (** Printing. *)
  let rec to_string (e: t): string =
    let parens_string_of_expression e =
      match e with
      | Binary _ | Unary _ -> "(" ^ to_string e ^ ")"
      | _ -> to_string e
    in
    match e with
    | Var (_,x) -> x.name
    | Prim (_,l) -> Literal.to_string l
    | Binary (_,op,e0,e1) ->
        let pe0 = parens_string_of_expression e0
        and pe1 = parens_string_of_expression e1 in
        pe0 ^ " " ^ op ^ " " ^ pe1
    | Unary (_,op,e) ->
        let pe = parens_string_of_expression e in
        op ^ " " ^ pe

  (** Return the position. *)
  let position (e: t): Positions.position =
    match e with
    | Var (p,_) | Prim (p,_)
    | Binary (p,_,_,_) | Unary (p,_,_) -> p

end


(** Linear expressions. *)
module Linexpr = struct

  (** Type of linear expressions. *)
  type t = {
    terms: (string * Literal.t) list;
    constant: Literal.t
  }

  (** Sum two linear expressions. *)
  let add (e0: t) (e1: t): t =
    let rec update v f = function
    | [] -> (false, [])
    | (v', c')::vs ->
        if v = v' then
          let c'' = f c' in
          (true, if Literal.eq0 c'' then vs else (v', c'')::vs)
        else
          let (b, vs) = update v f vs in
          (b, (v',c')::vs)
    in
    let ts0 = List.fold_left (fun ts (v, c) ->
      let b, ts = update v (fun c' -> Literal.add c c') ts in
      if b then ts else (v, c)::ts
    ) e0.terms e1.terms in
    { terms = ts0;
      constant = Literal.add e0.constant e1.constant }


  (** Multiply a linear expression by a constant. *)
  let mul (e: t) (a: Literal.t): t =
    { terms = List.map (fun (v,c) -> (v, Literal.mul a c)) e.terms;
      constant = Literal.mul a e.constant }


  (** Substract two linear expressions. *)
  let sub (e0: t) (e1: t): t =
    add e0 (mul e1 (Int (-1)))


  let minus (e: t): t =
    mul e (Int (-1))


  (** Normalize a comparison between linear expressions. *)
  let comp (p: Positions.position) (op: string) (e0: t) (e1: t): (t * string) =
    match op with
    | "<=" -> (sub e1 e0, ">=") | "<" -> (sub e1 e0, ">")
    | "==" | "!=" | ">=" | ">" -> (sub e0 e1, op)
    | _ -> Errors.fatal' p ("Illegal operator " ^ op)
 

  (** Reverse a comparison. *)
  let rev (e,op: t * string): t * string =
    match op with
    | ">" ->  (minus e, ">=") | ">=" -> (minus e, ">")
    | "==" -> (e, "!=") | "!=" -> (e, "==")
    | _ -> assert false (* Other operators eliminated by function 'comp' *)


  (** Printing. *)
  let to_string (e: t): string =
    let pc =
      if Literal.eq0 e.constant then "" else Literal.to_string e.constant
    in
    let pe = List.fold_left (fun s (x, c) ->
      if s = "" then
        match c with
        | Int 0 -> "" | Int 1 -> x | Int (-1) -> "-" ^ x
        | Float 0. -> "" | Float 1. -> x | Float (-1.) -> "-" ^ x
        | _ -> Literal.to_string c ^ x
      else
        match c with
        | Int 0 -> s | Int 1 -> s ^ " + " ^ x | Int (-1) -> s ^ " - " ^ x
        | Float 0. -> s | Float 1. -> s ^ " + " ^ x | Float (-1.) -> s ^ " - " ^ x
        | c when Literal.lt0 c -> s ^ " - " ^ Literal.to_string (Literal.neg c) ^ x
        | _ -> s ^ " + " ^ Literal.to_string c ^ x  
    ) pc e.terms
    in
    if pe = "" then "0" else pe

end


(** Boolean expressions. *)
module Bexpr = struct
  type 'a t =
    Atom of 'a
  | Conj of 'a t list 
  | Disj of 'a t list
  | Top                                 (* Sat expression. *)
  | Bot                                 (* Unsat expression. *)


  (** And operator. *)
  let band (e0: 'a t) (e1: 'a t): 'a t =
    match (e0, e1) with
    | Top, _ -> e1 | _, Top -> e0
    | Bot, _ -> Bot | _, Bot -> Bot
    | Conj bs, Conj bs' -> Conj (bs @ bs')
    | Conj bs, _ -> Conj (e1::bs)
    | _, Conj bs -> Conj (e0::bs)
    | _ -> Conj [e0;e1]


  (** Or operator. *)
  let bor (e0: 'a t) (e1: 'a t): 'a t =
    match (e0, e1) with
    | Top, _ -> Top | _, Top -> Top
    | Bot, _ -> e1 | _, Bot -> e0
    | Disj bs, Disj bs' -> Disj (bs @ bs')
    | Disj bs, _ -> Disj (e1::bs)
    | _, Disj bs -> Disj (e0::bs)
    | _ -> Disj [e0;e1] 


  (** Neg operator. *)
  let rec bnot (fnot: 'a -> 'a) (e: 'a t): 'a t =
    match e with
    | Top -> Bot | Bot -> Top
    | Conj bs -> Disj (List.map (bnot fnot) bs)
    | Disj bs -> Conj (List.map (bnot fnot) bs)
    | Atom a -> Atom (fnot a)


  (** Printing. *)
  let rec to_string (string_of_a: 'a -> string) (e: 'a t): string =
    let pdisj e =
      match e with
      | Disj _ -> "(" ^ to_string string_of_a e ^ ")" 
      | _ -> to_string string_of_a e
    in
    let pconj e =
      match e with
      | Conj _ -> "(" ^ to_string string_of_a e ^ ")" 
      | _ -> to_string string_of_a e
    in

    match e with
    | Top -> "true" | Bot -> "false"
    | Atom a -> string_of_a a
    | Conj bs ->
        List.fold_left (fun s b ->
          if s = "" then pdisj b
          else s ^ " && " ^ pdisj b
        ) "" bs
    | Disj bs ->
        List.fold_left (fun s b ->
          if s == "" then pconj b
          else s ^ " || " ^ pconj b
        ) "" bs

end
