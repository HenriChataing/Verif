(** Command line options parsing. *)

let verbose = ref false

let options = Arg.align [
  ("-v", Arg.Unit (fun _ -> verbose := true), "toggle verbose mode")
]

let message = "Usage: verif [options] input_file"

let filename =
  let filename = ref None in
  Arg.parse options (fun s -> filename := Some s) message;
  match !filename with
    | None -> Errors.fatal [] "No input file."
    | Some filename -> filename

