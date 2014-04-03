(** Lexical analysis. *)

{
  open SmtTokens

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
let alphanum = ['a'-'z' 'A'-'Z' '0'-'9']
let symbolchar = ['a'-'z' 'A'-'Z' '0'-'9'
                  '~' '!' '@' '$' '%' '^'
                  '&' '*' '_' '-' '+' '='
                  '<' '>' '.' '?' '/']

let symbol = (symbolchar+ | '|' symbolchar+ '|')
let keyword = ':' symbolchar+

rule token = parse

  (** Layout *)

  | newline { new_line lexbuf; token lexbuf }
  | blank+  { token lexbuf }
  | '"'     { string (Buffer.create 13) lexbuf }
  | ";"     { comment lexbuf }

  (** Keywords *)

  | "let" { LET }
  | "forall" { FORALL }
  | "exists" { EXISTS }
  | "par" { PAR }
  | "!" { BANG }
  | "as" { AS }
  | "_" { UNDERSCORE }
  | "NUMERAL" { NUMERAL }
  | "DECIMAL" { DECIMAL }
  | "STRING" { STRING }

  | "set-logic" { SETLOGIC }
  | "declare-fun" { DECLAREFUN }
  | "define-fun" { DEFINEFUN }
  | "declare-sort" { DECLARESORT }
  | "define_sort" { DEFINESORT }
  | "assert" { ASSERT }
  | "get-assertions" { GETASSERTIONS }
  | "check-sat" { CHECKSAT }
  | "get-proof" { GETPROOF }
  | "get-unsat-core" { GETUNSATCORE }
  | "get-value" { GETVALUE }
  | "get-assignment" { GETASSIGNMENT }
  | "push" { PUSH }
  | "pop" { POP }
  | "get-option" { GETOPTION }
  | "set-option" { SETOPTION }
  | "get-info" { GETINFO }
  | "set-info" { SETINFO }
  | "exit" { EXIT }

  (** Punctuation. *)

  | "(" { LPAREN }
  | ")" { RPAREN }
  | eof { EOF }

  (** Literals. *)
  | ('0' | ['1'-'9']['0'-'9']*)                  as i { NUM (int_of_string i) }
  | ('0' | ['1'-'9']['0'-'9']*) ['.'] ['0'-'9']+ as d { DEC d }
  | "#b" ['0' '1']+                              as b { BIN b }
  | "#x" ['0'-'9' 'A'-'F' 'a'-'f']+              as h { HEX h }
  | symbol as s { SYMBOL s }
  | keyword as k { KEYWORD k }

  (** Identifiers. *)

  | symbol as s  { SYMBOL s }

  | _  { lexical_error lexbuf "Invalid character." }

and comment = parse
  | eof { EOF }
  | newline { new_line lexbuf; token lexbuf }
  | _ { comment lexbuf }

and string buffer = parse
  | '"' { STR (Buffer.contents buffer) }
  | eof { lexical_error lexbuf "Unterminated string." }
  | _ as c { Buffer.add_char buffer c; string buffer lexbuf }

