(** Syntactic analysis. *)

%{
  open Positions
  open SmtSyntax
  open SmtTokens
%}

(** Punctuation. *)
%token LPAREN RPAREN EOF UNDERSCORE

(** Identifiers. *)
%token<string> SYMBOL KEYWORD

(** Literals. *)
%token<int> NUM
%token<string> DEC BIN HEX STR

(** Keywords. *)
%token LET FORALL EXISTS BANG AS
(* %token NUMERAL DECIMAL STRING PAR *)
%token SETLOGIC EXIT
%token DECLAREFUN DEFINEFUN DECLARESORT DEFINESORT
%token ASSERT GETASSERTIONS CHECKSAT
%token GETPROOF GETUNSATCORE GETVALUE GETASSIGNMENT
%token PUSH POP
%token GETOPTION SETOPTION GETINFO SETINFO

%start<SmtSyntax.command list> program

%%

program: cs = nonempty_list(command) EOF {
    cs
  }
| error {
    Errors.fatal [$startpos; $endpos] "Syntax error"
  }

command:
  LPAREN SETLOGIC sym=symbol RPAREN {
    SetLogic (lex_join $startpos $endpos, sym)
  }
| LPAREN DECLAREFUN sym=symbol LPAREN args=sort_list RPAREN sort=sort RPAREN {
    DeclareFun (lex_join $startpos $endpos, sym,args,sort)
  }
| LPAREN DEFINEFUN sym=symbol LPAREN args=arg_list RPAREN sort=sort expr=expression RPAREN {
    DefineFun (lex_join $startpos $endpos, sym,args,sort,expr)
  }
| LPAREN DECLARESORT sym=symbol n=NUM RPAREN {
    DeclareSort (lex_join $startpos $endpos, sym,n)
  }
| LPAREN DEFINESORT sym=symbol LPAREN syms=nonempty_list(symbol) RPAREN sort=sort RPAREN {
    DefineSort (lex_join $startpos $endpos, sym,syms,sort)
  }
| LPAREN ASSERT expr=expression RPAREN {
    Assert (lex_join $startpos $endpos, expr)
  }
| LPAREN GETASSERTIONS RPAREN { GetAssertions (lex_join $startpos $endpos) }
| LPAREN CHECKSAT RPAREN { CheckSat (lex_join $startpos $endpos) }
| LPAREN GETPROOF RPAREN { GetProof (lex_join $startpos $endpos) }
| LPAREN GETUNSATCORE RPAREN { GetUnsatCore (lex_join $startpos $endpos) }
| LPAREN GETVALUE exprs=nonempty_list(expression) RPAREN {
    GetValue (lex_join $startpos $endpos, exprs)
  }
| LPAREN GETASSIGNMENT RPAREN { GetAssignment (lex_join $startpos $endpos) }
| LPAREN PUSH n=NUM RPAREN { Push (lex_join $startpos $endpos, n) }
| LPAREN POP n=NUM RPAREN { Pop (lex_join $startpos $endpos, n) }
| LPAREN GETOPTION k=keyword RPAREN { GetOption (lex_join $startpos $endpos, k) }
| LPAREN SETOPTION a=attribute RPAREN { SetOption (lex_join $startpos $endpos, a) }
| LPAREN GETINFO k=keyword RPAREN { GetInfo (lex_join $startpos $endpos, k) }
| LPAREN SETINFO a=attribute RPAREN { SetInfo (lex_join $startpos $endpos, a) }
| LPAREN EXIT RPAREN { Exit (lex_join $startpos $endpos) }

symbol: s=SYMBOL { s }
keyword: k=KEYWORD { k }

sort_list:
  /*empty*/ { [] }
| s=sort ss=sort_list {
    s::ss
  }

arg_list:
  /*empty*/ { [] }
| LPAREN sy=symbol st=sort RPAREN ss=arg_list {
    (sy,st)::ss
  }

bind_list:
  /*empty*/ { [] }
| LPAREN s=symbol e=expression RPAREN bs=bind_list {
    (s,e)::bs
  }

identifier:
  sym=symbol {
    { symbol = sym; indexes = []; sort = None }
  }
| LPAREN UNDERSCORE sym=symbol ns=nonempty_list(NUM) RPAREN {
    { symbol = sym; indexes = ns; sort = None }
  }

qidentifier:
  LPAREN AS id=identifier s=sort RPAREN { id.sort <- Some s; id }
| id=identifier { id }

sort:
  id=identifier { Sort (id, []) }
| LPAREN id=identifier ss=nonempty_list(sort) RPAREN { Sort (id, ss) }

attribute:
  k=keyword a=attribute_value { (k, Some a) }
| k=keyword { (k, None) }

attribute_value:
  c=constant { VPrim c }
| sym=symbol { VSym sym }
| LPAREN attrs=nonempty_list(attribute_value) RPAREN { VApp attrs }

constant:
  n=NUM { Num n }
| d=DEC { Dec d }
| b=BIN { Bin b }
| h=HEX { Hex h }
| s=STR { Str s }

expression:
  c=constant { Prim (lex_join $startpos $endpos, c) }
| qid=qidentifier { Ident (lex_join $startpos $endpos, qid) }
| LPAREN sym=qidentifier es=nonempty_list(expression) RPAREN {
    App (lex_join $startpos $endpos, sym,es)
  }
| LPAREN FORALL LPAREN sys=arg_list RPAREN e=expression RPAREN {
    Forall (lex_join $startpos $endpos, sys,e)
  }
| LPAREN EXISTS LPAREN sys=arg_list RPAREN e=expression RPAREN {
    Exists (lex_join $startpos $endpos, sys,e)
  }
| LPAREN LET LPAREN bs=bind_list RPAREN e=expression RPAREN {
    Let (lex_join $startpos $endpos, bs,e)
  }
| LPAREN BANG e=expression attrs=nonempty_list(attribute) RPAREN {
    Attribute (lex_join $startpos $endpos, e,[])
  }

