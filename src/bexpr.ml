
(** Boolean expressions: formulae on atom propositions, which can be
    linear expressions for example. *)
type 'a bexpr =
    Atom of 'a
  | Conj of 'a bexpr list 
  | Disj of 'a bexpr list
  | Top                                 (* Sat expression. *)
  | Bot                                 (* Unsat expression. *)


(** And operator. *)
let band (e0: 'a bexpr) (e1: 'a bexpr): 'a bexpr =
  match (e0, e1) with
  | Top, _ -> e1 | _, Top -> e0
  | Bot, _ -> Bot | _, Bot -> Bot
  | Conj bs, Conj bs' -> Conj (bs @ bs')
  | Conj bs, _ -> Conj (e1::bs)
  | _, Conj bs -> Conj (e0::bs)
  | _ -> Conj [e0;e1]


(** Or operator. *)
let bor (e0: 'a bexpr) (e1: 'a bexpr): 'a bexpr =
  match (e0, e1) with
  | Top, _ -> Top | _, Top -> Top
  | Bot, _ -> e1 | _, Bot -> e0
  | Disj bs, Disj bs' -> Disj (bs @ bs')
  | Disj bs, _ -> Disj (e1::bs)
  | _, Disj bs -> Disj (e0::bs)
  | _ -> Disj [e0;e1] 


(** Neg operator. *)
let rec bnot (fnot: 'a -> 'a) (e: 'a bexpr): 'a bexpr =
  match e with
  | Top -> Bot | Bot -> Top
  | Conj bs -> Disj (List.map (bnot fnot) bs)
  | Disj bs -> Conj (List.map (bnot fnot) bs)
  | Atom a -> Atom (fnot a)

(** Printing. *)
let rec string_of_bexpr (string_of_a: 'a -> string) (e: 'a bexpr): string =
  let pdisj e =
    match e with
    | Disj _ -> "(" ^ string_of_bexpr string_of_a e ^ ")" 
    | _ -> string_of_bexpr string_of_a e
  in
  let pconj e =
    match e with
    | Conj _ -> "(" ^ string_of_bexpr string_of_a e ^ ")" 
    | _ -> string_of_bexpr string_of_a e
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

