type token =
  (** Literals and identifiers. *)

  | NUM of int
  | LID of string
  | INFIX0 of string
  | INFIX1 of string
  | INFIX2 of string
  | INFIX3 of string
  | MINUS
  | TRUE
  | FALSE

  (** Reserved words. *)

  | WHILE
  | IF
  | ELSE
  | BREAK
  | CONTINUE
  | ASSERT

  (** Puncutaion marks. *)

  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | SEMICOLON
  | EQUALS

  | EOF

