(** Command line options parsing. *)

let pprint_clauses = ref false
let dot_file = ref ""
let smt2_file = ref ""
let run_analysis = ref true
let value_domain = ref "box"


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

(* Check proposed domain. *)
let read_domain = Arg.String (fun s ->
  let domains = ["box"; "polka"] in
  let doms = List.filter (fun d ->
    try String.sub d 0 (String.length s) = s with _ -> false) domains in
  match doms with
  | [d] -> value_domain := d
  | _ -> raise (Arg.Bad "option: --domain: invalid domain")
)

let options = Arg.align [
  ("-v", Arg.Int (fun v -> Logger.set_verbose v), " toggle verbose mode");
  ("-c", Arg.Unit (fun _ -> pprint_clauses := true), " pretty print the horn clauses");
  ("--no-analysis", Arg.Unit (fun _ -> run_analysis := false), " perform only the simplifications");
  ("-d", read_dot, "FILE create a dot file describing the predicates' dependencies");
  ("--dot", read_dot, "FILE --");
  ("-s", read_smt2, "FILE  select the smt2 output file");
  ("--smt2", read_smt2, "FILE --");
  ("--verbose", Arg.String (fun m -> Logger.display m), "LVL set verbose level");
  ("--domain", read_domain, "DOM set the value domain (default box)")
]

let message = "Usage: verif [OPTIONS] FILE"

let filename =
  let filename = ref None in
  Arg.parse options (fun s -> filename := Some s) message;
  match !filename with
    | None -> Errors.fatal [] "No input file."
    | Some filename -> filename

let usage () = Arg.usage options message

