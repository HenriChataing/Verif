(** The front-end driver. *)

open Parser
open Lexer

type filename = Filename of string

let parse_program filename =
  let cin = open_in filename in
  let buf = Lexing.from_channel cin in
  Lexing.(buf.lex_curr_p <- { buf.lex_curr_p with pos_fname = filename });
  let ast = SmtParser.program SmtLexer.token buf in
  close_in cin;
  ast


(*
type ('a, 'b) pass = 'a -> filename -> 'b

let ( $> ) : ('a, 'b) pass -> ('b, 'c) pass -> ('a, 'c) pass = fun p1 p2 ->
  fun x filename ->
    let y = p1 x filename in
    p2 y filename

let save_as ext ?(check = ignore) f = fun x ((Filename origin) as ofilename) ->
  let (y, printed_y) = f x ofilename in
  let filename = Filename.chop_extension origin ^ ext in
  let cout = open_out filename in
  PPrint.ToChannel.pretty 0.8 100 cout printed_y;
  close_out cout;
  check filename;
  y

let parse : (unit, IAST.program) pass
= save_as ".mls" (fun () (Filename f) ->
  let iast = ASTio.IAST.parse_program f in
  (iast, ASTio.IAST.pprint_program iast)
)

let parse_explicitly_typed : (unit, XAST.program) pass
= save_as ".mlse" (fun () (Filename f) ->
  let xast = ASTio.XAST.parse_program f in
  (xast, ASTio.XAST.pprint_program xast)
)

let compile : (XAST.program, unit) pass
= save_as ".ml" (fun xast _ ->
  ((), ASTio.XAST.pprint_program_in_ocaml xast)
)

let process : unit = Options.(
  match filename with
    | EMH f ->
      (parse_explicitly_typed $> elaborate_dictionaries $> compile)
        () (Filename f)

    | MH f ->
      (parse $> elaborate_type_annotations $> elaborate_dictionaries $> compile)
        () (Filename f)
)*)
