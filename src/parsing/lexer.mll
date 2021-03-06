(** Lexical analysis. *)

{
  open Tokens

  let lexical_error lexbuf = Errors.fatal [lexbuf.Lexing.lex_curr_p]

  let new_line lexbuf = Lexing.(
    let cpos = lexbuf.lex_curr_p in
    let npos = { cpos with
      pos_lnum = cpos.pos_lnum + 1;
      pos_bol = cpos.pos_cnum;
    }
    in
    lexbuf.lex_curr_p <- npos
  )
}


let newline = ('\010' | '\013' | "\013\010")
let blank = [' ' '\009' '\012']
let lowalpha = ['a'-'z']
let alphanum = ['a'-'z' 'A'-'Z' '0'-'9']
let printchar = ['a'-'z' 'A'-'Z' '0'-'9' '_']
let infix0 = ['<' '>' '|' '&' '$']
let infix1 = ['@' '^']
let infix2 = ['+' '-']
let infix3 = ['*' '/' '%' '=']
let symbolchar = (infix0 | infix1 | infix2 | infix3 | ['%' '.' ':'])

let lid = (lowalpha | '_') printchar*

rule token = parse

  (** Layout *)

  | newline { new_line lexbuf; token lexbuf }
  | blank+  { token lexbuf }

  (** Keywords *)

  | "while" { WHILE }
  | "if" { IF }
  | "else" { ELSE }
  | "int" { INT }
  | "bool" { BOOL }
  | "float" { FLOAT }
  | "break" { BREAK }
  | "continue" { CONTINUE }
  | "true" { TRUE }
  | "false" { FALSE }

  (** Punctuation. *)

  | "/*" { OPEN_COMMENT }
  | "*/" { CLOSE_COMMENT }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "{" { LBRACE }
  | "}" { RBRACE }
  | ";" { SEMICOLON }
  | "=" { EQUALS }
  | "-" { MINUS }
  | eof { EOF }

  (** Identifiers. *)
  | lid as s { LID s }
  | ['0'-'9']+ as s { NUM (int_of_string s) }
  | ['0'-'9']+ '.' ['0'-'9']+ as s { DEC (float_of_string s) }
  | infix0 symbolchar* as s      { INFIX0 s }
  | infix1 symbolchar* as s      { INFIX1 s }
  | infix2 symbolchar* as s      { INFIX2 s }
  | infix3 symbolchar* as s      { INFIX3 s }

  (** Error. *)
  | _  { lexical_error lexbuf "Invalid character." }

