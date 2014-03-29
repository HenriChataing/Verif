(** Command line options parsing. *)

(* Toggle verbose mode. *)
val verbose: bool ref

type source =
  | EMH of string
  | MH of string

(** The filename that has been provided on the command line. *)
val filename: source
