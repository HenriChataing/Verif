(** Command line options parsing. *)

let options = Arg.align [
  ("-v", Arg.Unit (fun _ -> Logger.set_verbose 1), "toggle verbose mode");
  ("--verbose", Arg.Int (fun v -> Logger.set_verbose v), "set verbose level")
]

let message = "Usage: verif [options] input_file"

let filename =
  let filename = ref None in
  Arg.parse options (fun s -> filename := Some s) message;
  match !filename with
    | None -> Errors.fatal [] "No input file."
    | Some filename -> filename

