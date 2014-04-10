
(** Boolean expressions: formulae on atom propositions, which can be
    linear expressions for example. *)
type 'a bexpr =
    Atom of 'a
  | Conj of 'a bexpr list 
  | Disj of 'a bexpr list
  | Top                                 (* Sat expression. *)
  | Bot                                 (* Unsat expression. *)


(** And operator. *)
val band: 'a bexpr -> 'a bexpr -> 'a bexpr


(** Or operator. *)
val bor: 'a bexpr -> 'a bexpr -> 'a bexpr


(** Neg operator. *)
val bnot: ('a -> 'a) -> 'a bexpr -> 'a bexpr


(** Printing. *)
val string_of_bexpr: ('a -> string) -> 'a bexpr -> string

