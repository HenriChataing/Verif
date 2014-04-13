(** Contains the definition of several kinds of expressions. *)

open Positions

(** Literals. *)
type literal =
    Int of int
  | Bool of bool


let string_of_literal (l: literal): string =
  match l with
  | Int n -> string_of_int n
  | Bool true -> "true"
  | Bool false -> "false"




(** Basic expressions. *)
module Expr = struct
  (** Type of expressions. *)
  type t =
     Var of position * string
  | Prim of position * literal
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
    | Var (_,x) -> x
    | Prim (_,l) -> string_of_literal l
    | Binary (_,op,e0,e1) ->
        let pe0 = parens_string_of_expression e0
        and pe1 = parens_string_of_expression e1 in
        pe0 ^ " " ^ op ^ " " ^ pe1
    | Unary (_,op,e) ->
        let pe = parens_string_of_expression e in
        op ^ " " ^ pe

end


(** Linear expressions. *)
module Linexpr = struct

  (** Type of linear expressions. *)
  type t = {
    terms: (string * int) list;
    constant: int

  }

  (** Sum two linear expressions. *)
  let add (e0: t) (e1: t): t =
    let rec update v f = function
    | [] -> (false, [])
    | (v', c')::vs ->
        if v = v' then
          let c'' = f c' in
          (true, if c'' = 0 then vs else (v', c'')::vs)
        else
          let (b, vs) = update v f vs in
          (b, (v',c')::vs)
    in
    let ts0 = List.fold_left (fun ts (v, c) ->
      let b, ts = update v (fun c' -> c + c') ts in
      if b then ts else (v, c)::ts
    ) e0.terms e1.terms in
    { terms = ts0;
      constant = e0.constant + e1.constant }


  (** Multiply a linear expression by a constant. *)
  let mul (e: t) (a: int): t =
    { terms = List.map (fun (v,c) -> (v, a*c)) e.terms;
      constant = e.constant * a }


  (** Substract two linear expressions. *)
  let sub (e0: t) (e1: t): t =
    add e0 (mul e1 (-1))


  let minus (e: t): t =
    mul e (-1)


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
      if e.constant = 0 then "" else string_of_int e.constant
    in
    let pe = List.fold_left (fun s (x, c) ->
      if s = "" then
        match c with
        | 0 -> "" | 1 -> x | -1 -> "-" ^ x
        | _ -> string_of_int c ^ x
      else
        match c with
        | 0 -> s | 1 -> s ^ " + " ^ x | -1 -> s ^ " - " ^ x
        | c when c < 0 -> s ^ " - " ^ string_of_int (-c) ^ x
        | _ -> s ^ " + " ^ string_of_int c ^ x  
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
