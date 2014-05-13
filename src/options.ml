(** Command line options parsing. *)

let pprint_clauses = ref false
let dot_file = ref ""
let smt2_file = ref ""

(* Check proposed smt2 file. *)
let read_smt2 = Arg.String (fun s ->
  if Filename.check_suffix s ".smt2" then smt2_file := s
  else raise (Arg.Bad "option --smt2: invalid file")
)

(* Check proposed dot file. *)
let read_dot = Arg.String (fun s ->
  if Filename.check_suffix s ".dot" then dot_file := s
  else raise (Arg.Bad "option --dot: invalid file")
)

let options = Arg.align [
  ("-v", Arg.Unit (fun _ -> Logger.set_verbose 1), " toggle verbose mode");
  ("-c", Arg.Unit (fun _ -> pprint_clauses := true), " pretty print the horn clauses");
  ("-d", read_dot, "FILE create a dot file describing the predicates' dependencies");
  ("--dot", read_dot, "FILE --");
  ("-s", read_smt2, "FILE  select the smt2 output file");
  ("--smt2", read_smt2, "FILE --");
  ("--verbose", Arg.Int (fun v -> Logger.set_verbose v), "LVL set verbose level")
]

let message = "Usage: verif [OPTIONS] FILE"

let filename =
  let filename = ref None in
  Arg.parse options (fun s -> filename := Some s) message;
  match !filename with
    | None -> Errors.fatal [] "No input file."
    | Some filename -> filename

let usage () = Arg.usage options message

