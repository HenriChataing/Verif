(** The front-end driver. *)

open Cfg
open Labels
open Analysis
open Expressions
open Environment
open Horn

(** Parse a c program file. *)
let parse_c filename =
  let cin = open_in filename in
  let buf = Lexing.from_channel cin in
  Lexing.(buf.lex_curr_p <- { buf.lex_curr_p with pos_fname = filename });
  let ast = Parser.program Lexer.token buf in
  close_in cin;
  ast

(** Parse a SmtLib program file. *)
let parse_smt filename =
  let cin = open_in filename in
  let buf = Lexing.from_channel cin in
  Lexing.(buf.lex_curr_p <- { buf.lex_curr_p with pos_fname = filename });
  let ast = SmtParser.program SmtLexer.token buf in
  close_in cin;
  ast


(** Main function. Read the filename from the command line arguments, parse and process
    the file. *)
let main =
  let f = Options.filename in
  (* C files. *)
  if Filename.check_suffix f ".c" then begin
    let ast = typecheck (parse_c f) in
    let cfg = build_cfg ast in
    print_cfg cfg;
    let astate = analyze cfg in
    print_abstract astate
  (* Smt files. *)
  end else if Filename.check_suffix f ".smt2" then begin
    let ast = parse_smt f in
    let p = extract_hclauses ast in
    let cs = List.map simplify_hclause p.clauses in
    let p = simplify_hclauses { p with clauses = cs } in
    List.iter (fun c -> print_string (string_of_hclause c); print_newline ()) p.clauses;
  end else begin
    print_string "This file extension is not recognized by the program 'verif'";
    print_newline ()
  end

