(** Linear expressions: expressions of the form
      c + a1 * x1 + a2 * x2 + .. + an * xn
 *)

type linexpr = {
  terms: (string * int) list;
  constant: int
}

(** Sum two linear expressions. *)
val add: linexpr -> linexpr -> linexpr


(** Multiply a linear expression by a constant. *)
val mul: linexpr -> int -> linexpr


(** Substract two linear expressions. *)
val sub: linexpr -> linexpr -> linexpr


val minus: linexpr -> linexpr


(** Normalize a comparison between linear expressions. *)
val comp: Positions.position -> string -> linexpr -> linexpr -> linexpr * string
 

(** Reverse a comparison. *)
val rev: linexpr * string -> linexpr * string


(** Printing. *)
val string_of_linexpr: linexpr -> string

