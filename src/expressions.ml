(** Contains the definition of several kinds of expressions. *)

open Positions
open Literal
open Types

(** Variables. *)
type var = {
  vid: int;               (* Unique identifier. *)
  name: string;           (* The name of the variable. *)
  pos: position;          (* The place of declaration of the variable. *)
  mutable ptype: ptype    (* The type of declaration. *)
}


(** Basic expressions. *)
module Expr = struct
  (** Type of expressions. *)
  type t =
     Var of position * var
  | Prim of position * Literal.t
  | Binary of position * string * t * t
  | Unary of position * string * t
  | Clause of position * var * t list

  (** Return the free variables of an expression. *)
  let rec freevar (e: t): var list =
    match e with
    | Var (_, x) -> [x] | Prim _ -> [] 
    | Binary (_, _, e0, e1) -> freevar e0 @ freevar e1
    | Unary (_, _, e) -> freevar e
    | Clause (_,_, es) -> List.concat (List.map freevar es)

  (** Replace a subset of the variables. *)
  let rec subs (sub: (var * var) list) (e: t): t =
    match e with
    | Var (pos, x) ->
        begin try
          Var (pos, List.assoc x sub)
        with Not_found ->
          Var (pos, x)
        end
    | Prim _ -> e
    | Binary (pos, op, e0, e1) -> Binary (pos, op, subs sub e0, subs sub e1)
    | Unary (pos, op, e) -> Unary (pos, op, subs sub e)
    | Clause (pos, op, es) -> Clause (pos, op, List.map (subs sub) es)

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
    | Clause (_, c, []) ->
        c.name ^ "()"
    | Clause (_, c, e::es) ->
        c.name ^ "(" ^ to_string e ^ List.fold_left (fun s e -> s ^ ", " ^ to_string e) "" es ^ ")"

  (** Return the position. *)
  let position (e: t): Positions.position =
    match e with
    | Var (p,_) | Prim (p,_)
    | Binary (p,_,_,_) | Unary (p,_,_) | Clause (p,_,_) -> p
end


open Expr


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

  (** Negation. *)
  let minus (e: t): t =
    mul e (Int (-1))

  (** Replace a subset of the variables by other linear expressions. *)
  let subs (sub: (var * t) list) (e: t): t =
    List.fold_left (fun ans (x, l) ->
      try
        let _, le = List.find (fun (v, _) -> v.name = x) sub in
        add ans (mul le l)
      with Not_found -> 
        add ans { terms = [x,l]; constant = Int 0 }
    ) { terms = []; constant = e.constant } e.terms


  (** Rename the variables. *)
  let rename (renaming: (var * var) list) (e: t): t =
    { e with terms = List.map (fun (x, l) ->
        try
          let (_, x') = List.find (fun (v,_) -> v.name = x) renaming in
          x'.name, l
        with Not_found -> x, l
      ) e.terms }


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


  (** Conversion of expressions to linear expressions.
      Any non linear expression raises an error. *)
  let rec of_expr (e: Expr.t): t =
    match e with
    | Var (p, v) -> 
        { terms = [v.name, Int 1]; constant = Int 0 }
    | Binary (_, "+", e0, e1) -> add (of_expr e0) (of_expr e1)
    | Binary (_, "-", e0, e1) -> add (of_expr e0) (minus (of_expr e1))
    | Binary (_, "*", e, Prim (_, a)) -> mul (of_expr e) a
    | Binary (_, "*", Prim (_, a), e) -> mul (of_expr e) a
    | Unary (_, "-", e) -> minus (of_expr e)
    | Prim (_, a) -> { terms = []; constant = a }
    | _ -> Errors.fatal' (Expr.position e) "This expression is not linear"

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
  let rec bnot (fnot: 'a -> 'a t) (e: 'a t): 'a t =
    match e with
    | Top -> Bot | Bot -> Top
    | Conj bs -> Disj (List.map (bnot fnot) bs)
    | Disj bs -> Conj (List.map (bnot fnot) bs)
    | Atom a -> fnot a

  (** Normalize a comparison between linear expressions. *)
  let comp
      (p: Positions.position)
      (op: string)
      (e0: Linexpr.t)
      (e1: Linexpr.t): (Linexpr.t * string) t =
    match op with
    | "<=" -> Atom (Linexpr.sub e1 e0, ">=") | "<" -> Atom (Linexpr.sub e1 e0, ">") 
    | "==" | ">=" | ">" -> Atom (Linexpr.sub e0 e1, op)
    | "!=" ->
       let e = Linexpr.sub e0 e1 in
       Disj [
         Atom (e, ">");
         Atom (Linexpr.minus e, ">")
       ]
    | _ -> Errors.fatal' p ("Illegal operator " ^ op)

  (** Reverse a comparison. *)
  let rev (e,op: Linexpr.t * string): (Linexpr.t * string) t =
    match op with
    | ">" ->  Atom (Linexpr.minus e, ">=")
    | ">=" -> Atom (Linexpr.minus e, ">")
    | "==" -> Disj [Atom (e, ">"); Atom (Linexpr.minus e, ">")]
    | _ -> assert false (* Other operators eliminated by function 'comp' *)


  (** Printing. *)
  let rec to_string (string_of_a: 'a -> string) (e: 'a t): string =
    let pdisj e =
      match e with
      | Disj (_::_) -> "(" ^ to_string string_of_a e ^ ")" 
      | _ -> to_string string_of_a e
    in
    let pconj e =
      match e with
      | Conj (_::_) -> "(" ^ to_string string_of_a e ^ ")" 
      | _ -> to_string string_of_a e
    in

    match e with
    | Top -> "true" | Bot -> "false"
    | Atom a -> string_of_a a
    | Conj [] -> "" | Conj [b] -> to_string string_of_a b
    | Conj (b::bs) ->
        List.fold_left (fun s b ->
          s ^ " && " ^ pdisj b
        ) (pdisj b) bs
    | Disj [] -> "" | Disj [b] -> to_string string_of_a b
    | Disj (b::bs) ->
        List.fold_left (fun s b ->
          s ^ " || " ^ pconj b
        ) (pconj b) bs


  (** Conversion of expressions to boolean expressions.
      Any non boolean expression raises an error. *)
  let rec of_expr (e: Expr.t): (Linexpr.t * string) t =
    match e with
    | Binary (_, "&&", e0, e1) -> band (of_expr e0) (of_expr e1)
    | Binary (_, "||", e0, e1) -> bor (of_expr e0) (of_expr e1)
    | Binary (p, op, e0, e1) ->
        let le0 = Linexpr.of_expr e0
        and le1 = Linexpr.of_expr e1 in
        comp p op le0 le1
    | Prim (_, Bool b) -> if b then Top else Bot
    | _ -> Errors.fatal' (Expr.position e) "This expression is not boolean"

end

