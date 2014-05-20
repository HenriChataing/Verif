(** Contains the definition of several kinds of expressions. *)

open Positions
open Utils
open Primitive
open Types
open Vars

(** Basic expressions. *)
module Expr = struct
  (** Type of expressions. *)
  type t =
    Var of position * var
  | Prim of position * Primitive.t
  | Binary of position * string * t * t
  | Unary of position * string * t
  | Predicate of position * var * t list

  (** Return the free variables of an expression. *)
  let rec freevar (e: t): var list =
    match e with
    | Var (_, x) -> [x] | Prim _ -> []
    | Binary (_, _, e0, e1) -> freevar e0 @ freevar e1
    | Unary (_, _, e) -> freevar e
    | Predicate (_,_, es) -> List.concat (List.map freevar es)

  (** Return the type of an expression.
      This function does not perform any sort of type checking. *)
  let rec typeof (e: t): typ =
    match e with
    | Var (_, v) -> v.typ
    | Prim (_, p) -> Primitive.typeof p
    | Unary (_, "not", _) -> TyBool
    | Unary (_, "-", e) -> typeof e
    | Binary (_, "==", _,_) | Binary (_, "<>", _,_)
    | Binary (_, "<=", _,_) | Binary (_, ">=", _,_)
    | Binary (_, "<", _,_) | Binary (_, ">", _,_) -> TyBool
    | Binary (_, _, e0, e1) -> join (typeof e0) (typeof e1)
    | Predicate (_,_,_) -> TyBool
    | _ -> Errors.fatal [] "this expression is not typeable"

  (** Rename some of the variables. *)
  let rec rename (sub: (var * var) list) (e: t): t =
    match e with
    | Var (pos, x) ->
        begin try
          Var (pos, List.assoc x sub)
        with Not_found ->
          Var (pos, x)
        end
    | Prim _ -> e
    | Binary (pos, op, e0, e1) -> Binary (pos, op, rename sub e0, rename sub e1)
    | Unary (pos, op, e) -> Unary (pos, op, rename sub e)
    | Predicate (pos, op, es) -> Predicate (pos, op, List.map (rename sub) es)

  (** Substitute variables by expression. *)
  let rec subs (sub: (var * t) list) (e: t): t =
    match e with
    | Var (pos, x) ->
        begin try List.assoc x sub
        with Not_found -> e
        end
    | Binary (pos, op, e0, e1) -> Binary (pos, op, subs sub e0, subs sub e1)
    | Unary (pos, op, e) -> Unary (pos, op, subs sub e)
    | Predicate (pos, p, es) -> Predicate (pos, p, List.map (subs sub) es)
    | _ -> e

  (** Printing. *)
  let rec to_string (e: t): string =
    let parens_string_of_expression e =
      match e with
      | Binary _ | Unary _ -> "(" ^ to_string e ^ ")"
      | _ -> to_string e
    in
    match e with
    | Var (_,x) -> x.name ^ delimitor ^ string_of_int x.vid
    | Prim (_,l) -> Primitive.to_string l
    | Binary (_,op,e0,e1) ->
        let pe0 = parens_string_of_expression e0
        and pe1 = parens_string_of_expression e1 in
        pe0 ^ " " ^ op ^ " " ^ pe1
    | Unary (_,op,e) ->
        let pe = parens_string_of_expression e in
        op ^ " " ^ pe
    | Predicate (_, c, []) ->
        c.name ^ "()"
    | Predicate (_, c, e::es) ->
        c.name ^ "(" ^ to_string e ^ List.fold_left (fun s e -> s ^ ", " ^ to_string e) "" es ^ ")"


  (** Return the position. *)
  let position (e: t): Positions.position =
    match e with
    | Var (p,_) | Prim (p,_)
    | Binary (p,_,_,_) | Unary (p,_,_) | Predicate (p,_,_) -> p


  (** Boolean negation. The expression is expected to be of type bool. *)
  let rec enot (e: t): t =
    match e with
    (* Boolean constructs. *)
    | Binary (pos, "||", e0, e1) -> Binary (pos, "&&", enot e0, enot e1)
    | Binary (pos, "&&", e0, e1) -> Binary (pos, "||", enot e0, enot e1)
    | Unary (pos, "not", e) -> e
    | Prim (pos, Primitive.Bool b) -> Prim (pos, Primitive.Bool (not b))
    (* Comparators. *)
    | Binary (pos, "==", e0, e1) -> Binary (pos, "<>", e0, e1)
    | Binary (pos, "<>", e0, e1) -> Binary (pos, "==", e0, e1)
    | Binary (pos, "<=", e0, e1) -> Binary (pos, ">", e0, e1)
    | Binary (pos, ">", e0, e1) -> Binary (pos, "<=", e0, e1)
    | Binary (pos, ">=", e0, e1) -> Binary (pos, "<", e0, e1)
    | Binary (pos, "<", e0, e1) -> Binary (pos, ">=", e0, e1)
    (* Errors. *)
    | _ -> Errors.fatal' (position e) "This expression can not be negated."

  (** Return the list of predicates. *)
  let rec predicates (e: t): var list =
    match e with
    | Binary (_,_,e0,e1) -> predicates e0 @ predicates e1
    | Unary (_,_,e) -> predicates e
    | Predicate (_,p, _) -> [p] (* No recursion. *)
    | _ -> []
end


open Expr


(** Boolean expressions. *)
module Bexpr = struct
  (* The type of boolean expressions leaves the type of atoms
    undetermined, so as to be able to reuse the structure after
    conversion from expressions to apron linear expressions. *)
  type 'a t =
    Atom of 'a
  | Conj of 'a t list
  | Disj of 'a t list
  | Top                 (* Sat expression. *)
  | Bot                 (* Unsat expression. *)


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

  (** Neg operator. The negation is pushed to the leaves. *)
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
  let rec of_expr (e: Expr.t): (Expr.t * string, var * Expr.t list) either t =
    match e with
    | Prim (_, Bool b) -> if b then Top else Bot
    | Binary (_, "&&", e0, e1) -> band (of_expr e0) (of_expr e1)
    | Binary (_, "||", e0, e1) -> bor (of_expr e0) (of_expr e1)
    | Binary (p, op, e0, e1) ->
        Atom (Left (Binary (p, "-", e0, e1), op))
    | Predicate (_, c, es) ->
        Atom (Right (c, es))
    | _ -> Errors.fatal' (Expr.position e) "This expression is not boolean"

end

