(** Command line options parsing. *)

(** Pretty print the clauses. *)
val pprint_clauses: bool ref

(** Produce DOT file. *)
val dot_file: string ref

(** Selection of the smt2 output. *)
val smt2_file: string ref

(** The filename that has been provided on the command line. *)
val filename: string

(** Output usage information. *)
val usage: unit -> unit

