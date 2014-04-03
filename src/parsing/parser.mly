(** Syntactic analysis. *)

%{
  open Positions
  open Syntax
%}

(** Punctuation. *)
%token LPAREN RPAREN
%token LBRACE RBRACE
%token SEMICOLON EQUALS
%token MINUS
%token EOF

(** Identifiers. *)
%token<string> LID
%token<string> INFIX0 INFIX1 INFIX2 INFIX3

(** Literals. *)
%token<int> NUM

(** Keywords. *)
%token WHILE
%token IF ELSE
%token BREAK CONTINUE
%token TRUE FALSE

%left INFIX0
%right INFIX1
%left INFIX2 MINUS
%left INFIX3

%start<Syntax.block> program

%%

program: ins = nonempty_list(instruction) EOF {
    ins
  }
| error {
    Errors.fatal [$startpos; $endpos] "Syntax error"
  }


instruction:
  WHILE LPAREN e=expression RPAREN b=block SEMICOLON {
    While (e, b)
  }
| CONTINUE SEMICOLON {
    Continue
  }
| BREAK SEMICOLON {
    Break
  }
| IF LPAREN e=expression RPAREN b=block SEMICOLON {
    If (e, b, None)
  }
| IF LPAREN e=expression RPAREN b1=block ELSE b2=block SEMICOLON {
    If (e, b1, Some b2)
  }
| id=LID EQUALS e=expression SEMICOLON {
    Assign (id, e)
  }

block:
  ins=nonempty_list(instruction) {
    ins
  }
| LBRACE ins=nonempty_list(instruction) RBRACE {
    ins
  }

expression:
  a=atom { a }
| e1=expression op=INFIX0 e2=expression { Binary (op,e1,e2) }
| e1=expression op=INFIX1 e2=expression { Binary (op,e1,e2) }
| e1=expression op=INFIX2 e2=expression { Binary (op,e1,e2) }
| e1=expression op=INFIX3 e2=expression { Binary (op,e1,e2) }
| e1=expression MINUS e2=expression { Binary ("-",e1,e2) }
| MINUS e=expression { Unary ("-",e) }

atom:
  n=NUM { Prim (Int n) }
| TRUE { Prim (Bool true) }
| FALSE { Prim (Bool false) }
| id=LID { Var id }
| LPAREN e=expression RPAREN { e }

