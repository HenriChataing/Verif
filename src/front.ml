(** The front-end driver. *)

open Expressions
open Horn
open Analysis
open Generics
open Vars

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
    identify_widening_points p;
    simplify_clauses p;
    if !Options.dot_file <> "" then
      make_dot !Options.dot_file p;

    let g = convert_script p in
    (* Run anaysis and check results. *)
    let man = Polka.manager_alloc_loose () in
    let state = run_analysis man g in
    let ok = check_negatives man g state in
    if ok then print_string "sat" else print_string "unknown"; print_newline ();

    (* Output modified program. *)
    let ast = commands_of_script p in
    if !Options.pprint_clauses then begin
      for i=0 to (Array.length p.predicates)-1 do
        if p.predicates.(i).valid then List.iter (fun c -> print_string (string_of_clause c); print_newline ()) p.predicates.(i).clauses
      done;
      List.iter (fun c -> print_string (string_of_clause c); print_newline ()) p.negatives
    end;
    if !Options.smt2_file <> "" then
      let fmt = Format.formatter_of_out_channel (open_out !Options.smt2_file) in
      Smtlib.print_script fmt ast
  end else begin
    print_string "Invalid input file"; print_newline ();
    Options.usage ();
  end

