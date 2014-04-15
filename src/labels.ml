(** Definition of the labels of the control flow graph. *)

(** Id generator. *)
let label_counter = ref 0 ;;

type label = {
  id: int;
  position: Lexing.position
}

(** Create a new label identified by a position. *)
let new_label (p: Lexing.position): label =
  let newid = !label_counter in
  label_counter := newid + 1;
  { id = newid; position = p }

let undefined_label = {
  id = -1;
  position = Lexing.dummy_pos
}

let init_label = {
  id = 0;
  position = Lexing.dummy_pos
}

let string_of_label (l: label): string =
  "L" ^ string_of_int l.id

let reset_labels (): unit =
  label_counter := 0
