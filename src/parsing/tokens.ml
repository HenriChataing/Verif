type token =
  (** Literals and identifiers. *)

  | NUM of int
  | DEC of string
  | BIN of string
  | HEX of string
  | STR of string
  | SYMBOL of string
  | KEYWORD of string

  (** Reserved words. *)

  | LET
  | FORALL
  | EXISTS
  | PAR
  | BANG
  | AS
  | UNDERSCORE
  | NUMERAL
  | DECIMAL
  | STRING

  (** Command names. *)

  | SETLOGIC
  | DECLAREFUN
  | DEFINEFUN
  | DECLARESORT
  | DEFINESORT
  | ASSERT
  | GETASSERTIONS
  | CHECKSAT
  | GETPROOF
  | GETUNSATCORE
  | GETVALUE
  | GETASSIGNMENT
  | PUSH
  | POP
  | GETOPTION
  | SETOPTION
  | GETINFO
  | SETINFO
  | EXIT

  (** Puncutaion marks. *)

  | LPAREN
  | RPAREN
  | EOF
