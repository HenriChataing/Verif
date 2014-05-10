(** The front-end driver. *)

open Graph
open Expressions
open Horn

(** Parse a SmtLib program file. *)
let parse_smt filename =
  let cin = open_in filename in
  let buf = Lexing.from_channel cin in
  Lexing.(buf.lex_curr_p <- { buf.lex_curr_p with pos_fname = filename });
  let ast = Parser.program Lexer.token buf in
  close_in cin;
  ast


(** Main function. Read the filename from the command line arguments, parse and process
    the file. *)
let main =
  let f = Options.filename in
  (* Smt files. *)
  if Filename.check_suffix f ".smt2" then begin
    let ast = parse_smt f in
    let p = extract_clauses ast in
    simplify_clauses p;
    let g = build_graph p in
    identify_widening_points ~dot:!Options.dot_file g;
    let ast = commands_of_script p in
    if !Options.pprint_clauses then
      List.iter (fun c -> print_string (string_of_clause c); print_newline ()) p.clauses
    else
      Smtlib.print_script Format.std_formatter ast
  end else begin
    print_string "This file extension is not recognized by the program 'verif'";
    print_newline ()
  end

