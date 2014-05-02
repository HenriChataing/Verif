(** Syntactic analysis. *)

%{
  open Positions
  open Types
  open Literal
  open Expressions
  open Expr
  open Syntax
%}

(** Punctuation. *)
%token LPAREN RPAREN
%token LBRACE RBRACE
%token OPEN_COMMENT CLOSE_COMMENT
%token SEMICOLON EQUALS
%token MINUS
%token EOF

(** Identifiers. *)
%token<string> LID
%token<string> INFIX0 INFIX1 INFIX2 INFIX3

(** Literals. *)
%token<int> NUM
%token<float> DEC

(** Keywords. *)
%token WHILE
%token IF ELSE
%token INT BOOL FLOAT
%token BREAK CONTINUE
%token TRUE FALSE
%token ASSERT

%left INFIX0
%right INFIX1
%left INFIX2 MINUS
%left INFIX3
%nonassoc RPAREN
%nonassoc ELSE

%start<Syntax.block> program

%%

program: ins = nonempty_list(instruction) EOF {
    (lex_join $startpos $endpos, ins)
  }
| error {
    Errors.fatal [$startpos; $endpos] "Syntax error"
  }

instruction:
  OPEN_COMMENT e=expression CLOSE_COMMENT {
    Prove (lex_join $startpos $endpos, e)
  }
| WHILE LPAREN e=expression RPAREN b=block {
    While (lex_join $startpos $endpos, e, b)
  }
| CONTINUE SEMICOLON {
    Continue (lex_join $startpos $endpos)
  }
| BREAK SEMICOLON {
    Break (lex_join $startpos $endpos)
  }
| IF LPAREN e=expression RPAREN b=block {
    If (lex_join $startpos $endpos, e, b, None)
  }
| IF LPAREN e=expression RPAREN b1=block ELSE b2=block {
    If (lex_join $startpos $endpos, e, b1, Some b2)
  }
| t=ptype id=LID SEMICOLON {
    Declare (lex_join $startpos $endpos, { vid = 0; name = id; ptype = t }, None)
  }
| t=ptype id=LID EQUALS e=expression SEMICOLON {
    Declare (lex_join $startpos $endpos, { vid = 0; name = id; ptype = t }, Some e)
  }
| id=LID EQUALS e=expression SEMICOLON {
    Assign (lex_join $startpos $endpos, { vid = 0; name = id; ptype = TypeInt }, e)
  }
| ASSERT LPAREN e=expression RPAREN SEMICOLON {
    Assert (lex_join $startpos $endpos, e)
  }


block:
  ins=instruction {
    (lex_join $startpos $endpos, [ins])
  }
| LBRACE ins=nonempty_list(instruction) RBRACE {
    (lex_join $startpos $endpos, ins)
  }

expression:
  a=atom { a }
| e1=expression op=INFIX0 e2=expression { Binary (lex_join $startpos $endpos, op,e1,e2) }
| e1=expression op=INFIX1 e2=expression { Binary (lex_join $startpos $endpos, op,e1,e2) }
| e1=expression op=INFIX2 e2=expression { Binary (lex_join $startpos $endpos, op,e1,e2) }
| e1=expression op=INFIX3 e2=expression { Binary (lex_join $startpos $endpos, op,e1,e2) }
| e1=expression MINUS e2=expression { Binary (lex_join $startpos $endpos, "-",e1,e2) }
| MINUS e=expression { Unary (lex_join $startpos $endpos, "-",e) }

atom:
  n=NUM { Prim (lex_join $startpos $endpos, Int n) }
| d=DEC { Prim (lex_join $startpos $endpos, Float d) }
| TRUE { Prim (lex_join $startpos $endpos, Bool true) }
| FALSE { Prim (lex_join $startpos $endpos, Bool false) }
| id=LID { Var (lex_join $startpos $endpos, { vid = 0; name = id; ptype = TypeInt }) }
| LPAREN e=expression RPAREN { e }

ptype:
  INT { TypeInt }
| BOOL { TypeBool }
| FLOAT { TypeFloat }

