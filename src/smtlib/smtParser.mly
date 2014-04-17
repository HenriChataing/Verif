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
    SetLogic sym
  }
| LPAREN DECLAREFUN sym=symbol LPAREN args=sort_list RPAREN sort=sort RPAREN {
    DeclareFun (sym,args,sort)
  }
| LPAREN DEFINEFUN sym=symbol LPAREN args=arg_list RPAREN sort=sort expr=expression RPAREN {
    DefineFun (sym,args,sort,expr)
  }
| LPAREN DECLARESORT sym=symbol n=NUM RPAREN {
    DeclareSort (sym,n)
  }
| LPAREN DEFINESORT sym=symbol LPAREN syms=nonempty_list(symbol) RPAREN expr=expression RPAREN {
    DefineSort (sym,syms,expr)
  }
| LPAREN ASSERT expr=expression RPAREN {
    Assert expr
  }
| LPAREN GETASSERTIONS RPAREN { GetAssertions }
| LPAREN CHECKSAT RPAREN { CheckSat }
| LPAREN GETPROOF RPAREN { GetProof }
| LPAREN GETUNSATCORE RPAREN { GetUnsatCore }
| LPAREN GETVALUE exprs=nonempty_list(expression) RPAREN { GetValue exprs }
| LPAREN GETASSIGNMENT RPAREN { GetAssignment }
| LPAREN PUSH n=NUM RPAREN { Push n }
| LPAREN POP n=NUM RPAREN { Pop n }
| LPAREN GETOPTION k=keyword RPAREN { GetOption k }
| LPAREN SETOPTION k=keyword RPAREN { SetOption (k,()) }
| LPAREN GETINFO k=keyword RPAREN { GetInfo k }
| LPAREN SETINFO k=keyword RPAREN { SetInfo (k,()) }
| LPAREN EXIT RPAREN { Exit }

symbol: s=SYMBOL { Symbol s }
keyword: k=KEYWORD { Keyword k }

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
  sym=symbol { Simple sym }
| LPAREN UNDERSCORE sym=symbol ns=nonempty_list(NUM) RPAREN { Indexed (sym,ns) }

qidentifier:
  LPAREN AS id=identifier s=sort RPAREN { Qualified (id,s) }
| id=identifier { NonQualified id }

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
  c=constant { Prim c }
| qid=qidentifier { Ident qid }
| LPAREN sym=qidentifier es=nonempty_list(expression) RPAREN { App (sym,es) }
| LPAREN FORALL LPAREN sys=arg_list RPAREN e=expression RPAREN { Forall (sys,e) }
| LPAREN EXISTS LPAREN sys=arg_list RPAREN e=expression RPAREN { Exists (sys,e) }
| LPAREN LET LPAREN bs=bind_list RPAREN e=expression RPAREN { Let (bs,e) }
| LPAREN BANG e=expression attrs=nonempty_list(attribute) RPAREN { Attribute (e,[]) }

