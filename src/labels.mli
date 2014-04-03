(** Definition of the labels of the control flow graph. *)

type label = {
  id: int;
  position: Lexing.position
}

(** Create a new label identified by a position. *)
val new_label: Lexing.position -> label

val undefined_label: label
val string_of_label: label -> string
val reset_labels: unit -> unit
