exception Error

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> Tokens.token;
  _menhir_lexbuf: Lexing.lexbuf;
  mutable _menhir_token: Tokens.token;
  mutable _menhir_startp: Lexing.position;
  mutable _menhir_endp: Lexing.position;
  mutable _menhir_shifted: int
}

and _menhir_state = 
  | MenhirState152
  | MenhirState146
  | MenhirState142
  | MenhirState139
  | MenhirState137
  | MenhirState128
  | MenhirState127
  | MenhirState125
  | MenhirState120
  | MenhirState117
  | MenhirState116
  | MenhirState94
  | MenhirState91
  | MenhirState89
  | MenhirState84
  | MenhirState80
  | MenhirState78
  | MenhirState77
  | MenhirState76
  | MenhirState73
  | MenhirState71
  | MenhirState67
  | MenhirState64
  | MenhirState62
  | MenhirState60
  | MenhirState56
  | MenhirState52
  | MenhirState45
  | MenhirState43
  | MenhirState35
  | MenhirState34
  | MenhirState33
  | MenhirState32
  | MenhirState31
  | MenhirState30
  | MenhirState29
  | MenhirState25
  | MenhirState24
  | MenhirState22
  | MenhirState18
  | MenhirState0


# 3 "smtlib/parser.mly"
  
  open Positions
  open Syntax

# 62 "smtlib/parser.ml"
let _eRR =
  Error

let rec _menhir_goto_nonempty_list_attribute_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_nonempty_list_attribute_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState77 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv789 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_expression) * _menhir_state * 'tv_nonempty_list_attribute_) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv787 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_expression) * _menhir_state * 'tv_nonempty_list_attribute_) = _menhir_stack in
        let (_tok : Tokens.token) = _tok in
        ((match _tok with
        | Tokens.RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv783 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_expression) * _menhir_state * 'tv_nonempty_list_attribute_) = Obj.magic _menhir_stack in
            ((let _ = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv781 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_expression) * _menhir_state * 'tv_nonempty_list_attribute_) = Obj.magic _menhir_stack in
            ((let ((((_menhir_stack, _menhir_s), _), _, e), _, attrs) = _menhir_stack in
            let _v : 'tv_expression = 
# 128 "smtlib/parser.mly"
                                                                 ( Attribute (e,[]) )
# 89 "smtlib/parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v) : 'freshtv782)) : 'freshtv784)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv785 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_expression) * _menhir_state * 'tv_nonempty_list_attribute_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv786)) : 'freshtv788)) : 'freshtv790)
    | MenhirState89 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv793 * _menhir_state * 'tv_attribute) * _menhir_state * 'tv_nonempty_list_attribute_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv791 * _menhir_state * 'tv_attribute) * _menhir_state * 'tv_nonempty_list_attribute_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, x), _, xs) = _menhir_stack in
        let _v : 'tv_nonempty_list_attribute_ = 
# 126 "/home/egoergo/.opam/4.00.1/lib/menhir/standard.mly"
    ( x :: xs )
# 108 "smtlib/parser.ml"
         in
        _menhir_goto_nonempty_list_attribute_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv792)) : 'freshtv794)
    | _ ->
        _menhir_fail ()

and _menhir_goto_nonempty_list_sort_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_nonempty_list_sort_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState35 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv769 * _menhir_state * 'tv_sort) * _menhir_state * 'tv_nonempty_list_sort_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv767 * _menhir_state * 'tv_sort) * _menhir_state * 'tv_nonempty_list_sort_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, x), _, xs) = _menhir_stack in
        let _v : 'tv_nonempty_list_sort_ = 
# 126 "/home/egoergo/.opam/4.00.1/lib/menhir/standard.mly"
    ( x :: xs )
# 127 "smtlib/parser.ml"
         in
        _menhir_goto_nonempty_list_sort_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv768)) : 'freshtv770)
    | MenhirState34 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv779 * _menhir_state) * _menhir_state * 'tv_identifier) * _menhir_state * 'tv_nonempty_list_sort_) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv777 * _menhir_state) * _menhir_state * 'tv_identifier) * _menhir_state * 'tv_nonempty_list_sort_) = _menhir_stack in
        let (_tok : Tokens.token) = _tok in
        ((match _tok with
        | Tokens.RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv773 * _menhir_state) * _menhir_state * 'tv_identifier) * _menhir_state * 'tv_nonempty_list_sort_) = Obj.magic _menhir_stack in
            ((let _ = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv771 * _menhir_state) * _menhir_state * 'tv_identifier) * _menhir_state * 'tv_nonempty_list_sort_) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s), _, id), _, ss) = _menhir_stack in
            let _v : 'tv_sort = 
# 103 "smtlib/parser.mly"
                                                     ( Sort (id, ss) )
# 149 "smtlib/parser.ml"
             in
            _menhir_goto_sort _menhir_env _menhir_stack _menhir_s _v) : 'freshtv772)) : 'freshtv774)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv775 * _menhir_state) * _menhir_state * 'tv_identifier) * _menhir_state * 'tv_nonempty_list_sort_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv776)) : 'freshtv778)) : 'freshtv780)
    | _ ->
        _menhir_fail ()

and _menhir_goto_nonempty_list_NUM_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_nonempty_list_NUM_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState25 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv755 * _menhir_state * (
# 15 "smtlib/parser.mly"
      (int)
# 171 "smtlib/parser.ml"
        )) * _menhir_state * 'tv_nonempty_list_NUM_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv753 * _menhir_state * (
# 15 "smtlib/parser.mly"
      (int)
# 177 "smtlib/parser.ml"
        )) * _menhir_state * 'tv_nonempty_list_NUM_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, x), _, xs) = _menhir_stack in
        let _v : 'tv_nonempty_list_NUM_ = 
# 126 "/home/egoergo/.opam/4.00.1/lib/menhir/standard.mly"
    ( x :: xs )
# 183 "smtlib/parser.ml"
         in
        _menhir_goto_nonempty_list_NUM_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv754)) : 'freshtv756)
    | MenhirState24 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv765 * _menhir_state) * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 191 "smtlib/parser.ml"
        )) * _menhir_state * 'tv_nonempty_list_NUM_) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv763 * _menhir_state) * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 199 "smtlib/parser.ml"
        )) * _menhir_state * 'tv_nonempty_list_NUM_) = _menhir_stack in
        let (_tok : Tokens.token) = _tok in
        ((match _tok with
        | Tokens.RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv759 * _menhir_state) * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 208 "smtlib/parser.ml"
            )) * _menhir_state * 'tv_nonempty_list_NUM_) = Obj.magic _menhir_stack in
            ((let _ = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv757 * _menhir_state) * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 215 "smtlib/parser.ml"
            )) * _menhir_state * 'tv_nonempty_list_NUM_) = Obj.magic _menhir_stack in
            ((let ((((_menhir_stack, _menhir_s), _), s0), _, ns) = _menhir_stack in
            let _v : 'tv_identifier = let sym =
              let s = s0 in
              
# 72 "smtlib/parser.mly"
                         ( Symbol s )
# 223 "smtlib/parser.ml"
              
            in
            
# 95 "smtlib/parser.mly"
                                                            ( Indexed (sym,ns) )
# 229 "smtlib/parser.ml"
             in
            _menhir_goto_identifier _menhir_env _menhir_stack _menhir_s _v) : 'freshtv758)) : 'freshtv760)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv761 * _menhir_state) * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 239 "smtlib/parser.ml"
            )) * _menhir_state * 'tv_nonempty_list_NUM_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv762)) : 'freshtv764)) : 'freshtv766)
    | _ ->
        _menhir_fail ()

and _menhir_goto_attribute : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_attribute -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv751 * _menhir_state * 'tv_attribute) = Obj.magic _menhir_stack in
    ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv749 * _menhir_state * 'tv_attribute) = _menhir_stack in
    let (_tok : Tokens.token) = _tok in
    ((match _tok with
    | Tokens.KEYWORD _v ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
    | Tokens.RPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv747 * _menhir_state * 'tv_attribute) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : 'tv_nonempty_list_attribute_ = 
# 124 "/home/egoergo/.opam/4.00.1/lib/menhir/standard.mly"
    ( [ x ] )
# 266 "smtlib/parser.ml"
         in
        _menhir_goto_nonempty_list_attribute_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv748)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState89) : 'freshtv750)) : 'freshtv752)

and _menhir_goto_nonempty_list_attribute_value_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_nonempty_list_attribute_value_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState80 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv741 * _menhir_state) * _menhir_state * 'tv_nonempty_list_attribute_value_) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv739 * _menhir_state) * _menhir_state * 'tv_nonempty_list_attribute_value_) = _menhir_stack in
        let (_tok : Tokens.token) = _tok in
        ((match _tok with
        | Tokens.RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv735 * _menhir_state) * _menhir_state * 'tv_nonempty_list_attribute_value_) = Obj.magic _menhir_stack in
            ((let _ = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv733 * _menhir_state) * _menhir_state * 'tv_nonempty_list_attribute_value_) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, attrs) = _menhir_stack in
            let _v : 'tv_attribute_value = 
# 112 "smtlib/parser.mly"
                                                     ( VApp attrs )
# 297 "smtlib/parser.ml"
             in
            _menhir_goto_attribute_value _menhir_env _menhir_stack _menhir_s _v) : 'freshtv734)) : 'freshtv736)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv737 * _menhir_state) * _menhir_state * 'tv_nonempty_list_attribute_value_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv738)) : 'freshtv740)) : 'freshtv742)
    | MenhirState84 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv745 * _menhir_state * 'tv_attribute_value) * _menhir_state * 'tv_nonempty_list_attribute_value_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv743 * _menhir_state * 'tv_attribute_value) * _menhir_state * 'tv_nonempty_list_attribute_value_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, x), _, xs) = _menhir_stack in
        let _v : 'tv_nonempty_list_attribute_value_ = 
# 126 "/home/egoergo/.opam/4.00.1/lib/menhir/standard.mly"
    ( x :: xs )
# 316 "smtlib/parser.ml"
         in
        _menhir_goto_nonempty_list_attribute_value_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv744)) : 'freshtv746)
    | _ ->
        _menhir_fail ()

and _menhir_run79 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 12 "smtlib/parser.mly"
      (string)
# 325 "smtlib/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv731) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (s0 : (
# 12 "smtlib/parser.mly"
      (string)
# 335 "smtlib/parser.ml"
    )) = _v in
    ((let _v : 'tv_attribute_value = let sym =
      let s = s0 in
      
# 72 "smtlib/parser.mly"
                         ( Symbol s )
# 342 "smtlib/parser.ml"
      
    in
    
# 111 "smtlib/parser.mly"
             ( VSym sym )
# 348 "smtlib/parser.ml"
     in
    _menhir_goto_attribute_value _menhir_env _menhir_stack _menhir_s _v) : 'freshtv732)

and _menhir_run80 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv729 * _menhir_state) = _menhir_stack in
    let (_tok : Tokens.token) = _tok in
    ((match _tok with
    | Tokens.BIN _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
    | Tokens.DEC _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
    | Tokens.HEX _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
    | Tokens.LPAREN ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | Tokens.NUM _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
    | Tokens.STR _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
    | Tokens.SYMBOL _v ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState80) : 'freshtv730)

and _menhir_goto_nonempty_list_expression_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_nonempty_list_expression_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState91 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv713 * _menhir_state) * _menhir_state * 'tv_qidentifier) * _menhir_state * 'tv_nonempty_list_expression_) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv711 * _menhir_state) * _menhir_state * 'tv_qidentifier) * _menhir_state * 'tv_nonempty_list_expression_) = _menhir_stack in
        let (_tok : Tokens.token) = _tok in
        ((match _tok with
        | Tokens.RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv707 * _menhir_state) * _menhir_state * 'tv_qidentifier) * _menhir_state * 'tv_nonempty_list_expression_) = Obj.magic _menhir_stack in
            ((let _ = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv705 * _menhir_state) * _menhir_state * 'tv_qidentifier) * _menhir_state * 'tv_nonempty_list_expression_) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s), _, sym), _, es) = _menhir_stack in
            let _v : 'tv_expression = 
# 124 "smtlib/parser.mly"
                                                             ( App (sym,es) )
# 402 "smtlib/parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v) : 'freshtv706)) : 'freshtv708)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv709 * _menhir_state) * _menhir_state * 'tv_qidentifier) * _menhir_state * 'tv_nonempty_list_expression_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv710)) : 'freshtv712)) : 'freshtv714)
    | MenhirState94 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv717 * _menhir_state * 'tv_expression) * _menhir_state * 'tv_nonempty_list_expression_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv715 * _menhir_state * 'tv_expression) * _menhir_state * 'tv_nonempty_list_expression_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, x), _, xs) = _menhir_stack in
        let _v : 'tv_nonempty_list_expression_ = 
# 126 "/home/egoergo/.opam/4.00.1/lib/menhir/standard.mly"
    ( x :: xs )
# 421 "smtlib/parser.ml"
         in
        _menhir_goto_nonempty_list_expression_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv716)) : 'freshtv718)
    | MenhirState18 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv727 * _menhir_state) * _menhir_state * 'tv_nonempty_list_expression_) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv725 * _menhir_state) * _menhir_state * 'tv_nonempty_list_expression_) = _menhir_stack in
        let (_tok : Tokens.token) = _tok in
        ((match _tok with
        | Tokens.RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv721 * _menhir_state) * _menhir_state * 'tv_nonempty_list_expression_) = Obj.magic _menhir_stack in
            ((let _ = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv719 * _menhir_state) * _menhir_state * 'tv_nonempty_list_expression_) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, exprs) = _menhir_stack in
            let _v : 'tv_command = 
# 62 "smtlib/parser.mly"
                                                         ( GetValue exprs )
# 443 "smtlib/parser.ml"
             in
            _menhir_goto_command _menhir_env _menhir_stack _menhir_s _v) : 'freshtv720)) : 'freshtv722)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv723 * _menhir_state) * _menhir_state * 'tv_nonempty_list_expression_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv724)) : 'freshtv726)) : 'freshtv728)
    | _ ->
        _menhir_fail ()

and _menhir_run78 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 12 "smtlib/parser.mly"
      (string)
# 459 "smtlib/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv703 * _menhir_state * (
# 12 "smtlib/parser.mly"
      (string)
# 468 "smtlib/parser.ml"
    )) = _menhir_stack in
    let (_tok : Tokens.token) = _tok in
    ((match _tok with
    | Tokens.BIN _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
    | Tokens.DEC _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
    | Tokens.HEX _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
    | Tokens.LPAREN ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | Tokens.NUM _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
    | Tokens.STR _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
    | Tokens.SYMBOL _v ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
    | Tokens.KEYWORD _ | Tokens.RPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv701 * _menhir_state * (
# 12 "smtlib/parser.mly"
      (string)
# 491 "smtlib/parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, k0) = _menhir_stack in
        let _v : 'tv_attribute = let k =
          let k = k0 in
          
# 73 "smtlib/parser.mly"
                           ( Keyword k )
# 499 "smtlib/parser.ml"
          
        in
        
# 107 "smtlib/parser.mly"
            ( (k, None) )
# 505 "smtlib/parser.ml"
         in
        _menhir_goto_attribute _menhir_env _menhir_stack _menhir_s _v) : 'freshtv702)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState78) : 'freshtv704)

and _menhir_goto_qidentifier : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_qidentifier -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState146 | MenhirState128 | MenhirState120 | MenhirState18 | MenhirState94 | MenhirState91 | MenhirState76 | MenhirState73 | MenhirState67 | MenhirState56 | MenhirState45 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv695 * _menhir_state * 'tv_qidentifier) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv693 * _menhir_state * 'tv_qidentifier) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, qid) = _menhir_stack in
        let _v : 'tv_expression = 
# 123 "smtlib/parser.mly"
                  ( Ident qid )
# 526 "smtlib/parser.ml"
         in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v) : 'freshtv694)) : 'freshtv696)
    | MenhirState22 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv699 * _menhir_state) * _menhir_state * 'tv_qidentifier) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv697 * _menhir_state) * _menhir_state * 'tv_qidentifier) = _menhir_stack in
        let (_tok : Tokens.token) = _tok in
        ((match _tok with
        | Tokens.BIN _v ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
        | Tokens.DEC _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
        | Tokens.HEX _v ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
        | Tokens.LPAREN ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | Tokens.NUM _v ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
        | Tokens.STR _v ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
        | Tokens.SYMBOL _v ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState91) : 'freshtv698)) : 'freshtv700)
    | _ ->
        _menhir_fail ()

and _menhir_goto_sort : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_sort -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState35 | MenhirState34 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv653 * _menhir_state * 'tv_sort) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv651 * _menhir_state * 'tv_sort) = _menhir_stack in
        let (_tok : Tokens.token) = _tok in
        ((match _tok with
        | Tokens.LPAREN ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | Tokens.SYMBOL _v ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
        | Tokens.RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv649 * _menhir_state * 'tv_sort) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, x) = _menhir_stack in
            let _v : 'tv_nonempty_list_sort_ = 
# 124 "/home/egoergo/.opam/4.00.1/lib/menhir/standard.mly"
    ( [ x ] )
# 583 "smtlib/parser.ml"
             in
            _menhir_goto_nonempty_list_sort_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv650)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState35) : 'freshtv652)) : 'freshtv654)
    | MenhirState32 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv663 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_identifier) * _menhir_state * 'tv_sort) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv661 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_identifier) * _menhir_state * 'tv_sort) = _menhir_stack in
        let (_tok : Tokens.token) = _tok in
        ((match _tok with
        | Tokens.RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv657 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_identifier) * _menhir_state * 'tv_sort) = Obj.magic _menhir_stack in
            ((let _ = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv655 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_identifier) * _menhir_state * 'tv_sort) = Obj.magic _menhir_stack in
            ((let ((((_menhir_stack, _menhir_s), _), _, id), _, s) = _menhir_stack in
            let _v : 'tv_qidentifier = 
# 98 "smtlib/parser.mly"
                                        ( Qualified (id,s) )
# 609 "smtlib/parser.ml"
             in
            _menhir_goto_qidentifier _menhir_env _menhir_stack _menhir_s _v) : 'freshtv656)) : 'freshtv658)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv659 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_identifier) * _menhir_state * 'tv_sort) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv660)) : 'freshtv662)) : 'freshtv664)
    | MenhirState62 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv673 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 624 "smtlib/parser.ml"
        )) * _menhir_state * 'tv_sort) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv671 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 632 "smtlib/parser.ml"
        )) * _menhir_state * 'tv_sort) = _menhir_stack in
        let (_tok : Tokens.token) = _tok in
        ((match _tok with
        | Tokens.RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv667 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 641 "smtlib/parser.ml"
            )) * _menhir_state * 'tv_sort) = Obj.magic _menhir_stack in
            ((let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv665 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 648 "smtlib/parser.ml"
            )) * _menhir_state * 'tv_sort) = _menhir_stack in
            let (_tok : Tokens.token) = _tok in
            ((match _tok with
            | Tokens.LPAREN ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | Tokens.RPAREN ->
                _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState64) : 'freshtv666)) : 'freshtv668)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv669 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 667 "smtlib/parser.ml"
            )) * _menhir_state * 'tv_sort) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv670)) : 'freshtv672)) : 'freshtv674)
    | MenhirState127 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv677 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 676 "smtlib/parser.ml"
        )) * _menhir_state * 'tv_arg_list) * _menhir_state * 'tv_sort) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv675 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 684 "smtlib/parser.ml"
        )) * _menhir_state * 'tv_arg_list) * _menhir_state * 'tv_sort) = _menhir_stack in
        let (_tok : Tokens.token) = _tok in
        ((match _tok with
        | Tokens.BIN _v ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _v
        | Tokens.DEC _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _v
        | Tokens.HEX _v ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _v
        | Tokens.LPAREN ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | Tokens.NUM _v ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _v
        | Tokens.STR _v ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _v
        | Tokens.SYMBOL _v ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState128) : 'freshtv676)) : 'freshtv678)
    | MenhirState139 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv687 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 711 "smtlib/parser.ml"
        )) * _menhir_state * 'tv_sort_list) * _menhir_state * 'tv_sort) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv685 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 719 "smtlib/parser.ml"
        )) * _menhir_state * 'tv_sort_list) * _menhir_state * 'tv_sort) = _menhir_stack in
        let (_tok : Tokens.token) = _tok in
        ((match _tok with
        | Tokens.RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv681 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 728 "smtlib/parser.ml"
            )) * _menhir_state * 'tv_sort_list) * _menhir_state * 'tv_sort) = Obj.magic _menhir_stack in
            ((let _ = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv679 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 735 "smtlib/parser.ml"
            )) * _menhir_state * 'tv_sort_list) * _menhir_state * 'tv_sort) = Obj.magic _menhir_stack in
            ((let ((((_menhir_stack, _menhir_s), s0), _, args), _, sort) = _menhir_stack in
            let _v : 'tv_command = let sym =
              let s = s0 in
              
# 72 "smtlib/parser.mly"
                         ( Symbol s )
# 743 "smtlib/parser.ml"
              
            in
            
# 43 "smtlib/parser.mly"
                                                                             (
    DeclareFun (sym,args,sort)
  )
# 751 "smtlib/parser.ml"
             in
            _menhir_goto_command _menhir_env _menhir_stack _menhir_s _v) : 'freshtv680)) : 'freshtv682)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv683 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 761 "smtlib/parser.ml"
            )) * _menhir_state * 'tv_sort_list) * _menhir_state * 'tv_sort) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv684)) : 'freshtv686)) : 'freshtv688)
    | MenhirState142 | MenhirState137 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv691 * _menhir_state * 'tv_sort) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv689 * _menhir_state * 'tv_sort) = _menhir_stack in
        let (_tok : Tokens.token) = _tok in
        ((match _tok with
        | Tokens.LPAREN ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState142
        | Tokens.SYMBOL _v ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _v
        | Tokens.RPAREN ->
            _menhir_reduce63 _menhir_env (Obj.magic _menhir_stack) MenhirState142
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState142) : 'freshtv690)) : 'freshtv692)
    | _ ->
        _menhir_fail ()

and _menhir_run25 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 15 "smtlib/parser.mly"
      (int)
# 790 "smtlib/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv647 * _menhir_state * (
# 15 "smtlib/parser.mly"
      (int)
# 799 "smtlib/parser.ml"
    )) = _menhir_stack in
    let (_tok : Tokens.token) = _tok in
    ((match _tok with
    | Tokens.NUM _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | Tokens.RPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv645 * _menhir_state * (
# 15 "smtlib/parser.mly"
      (int)
# 810 "smtlib/parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : 'tv_nonempty_list_NUM_ = 
# 124 "/home/egoergo/.opam/4.00.1/lib/menhir/standard.mly"
    ( [ x ] )
# 816 "smtlib/parser.ml"
         in
        _menhir_goto_nonempty_list_NUM_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv646)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState25) : 'freshtv648)

and _menhir_goto_bind_list : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_bind_list -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState52 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv633 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 833 "smtlib/parser.ml"
        )) * _menhir_state * 'tv_expression) * _menhir_state * 'tv_bind_list) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv631 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 839 "smtlib/parser.ml"
        )) * _menhir_state * 'tv_expression) * _menhir_state * 'tv_bind_list) = Obj.magic _menhir_stack in
        ((let ((((_menhir_stack, _menhir_s), s0), _, e), _, bs) = _menhir_stack in
        let _v : 'tv_bind_list = let s =
          let s = s0 in
          
# 72 "smtlib/parser.mly"
                         ( Symbol s )
# 847 "smtlib/parser.ml"
          
        in
        
# 89 "smtlib/parser.mly"
                                                   (
    (s,e)::bs
  )
# 855 "smtlib/parser.ml"
         in
        _menhir_goto_bind_list _menhir_env _menhir_stack _menhir_s _v) : 'freshtv632)) : 'freshtv634)
    | MenhirState43 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv643 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_bind_list) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv641 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_bind_list) = _menhir_stack in
        let (_tok : Tokens.token) = _tok in
        ((match _tok with
        | Tokens.RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv637 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_bind_list) = Obj.magic _menhir_stack in
            ((let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv635 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_bind_list) = _menhir_stack in
            let (_tok : Tokens.token) = _tok in
            ((match _tok with
            | Tokens.BIN _v ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
            | Tokens.DEC _v ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
            | Tokens.HEX _v ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
            | Tokens.LPAREN ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState56
            | Tokens.NUM _v ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
            | Tokens.STR _v ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
            | Tokens.SYMBOL _v ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState56) : 'freshtv636)) : 'freshtv638)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv639 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_bind_list) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv640)) : 'freshtv642)) : 'freshtv644)
    | _ ->
        _menhir_fail ()

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_attribute_value : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_attribute_value -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState84 | MenhirState80 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv625 * _menhir_state * 'tv_attribute_value) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv623 * _menhir_state * 'tv_attribute_value) = _menhir_stack in
        let (_tok : Tokens.token) = _tok in
        ((match _tok with
        | Tokens.BIN _v ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
        | Tokens.DEC _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
        | Tokens.HEX _v ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
        | Tokens.LPAREN ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | Tokens.NUM _v ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
        | Tokens.STR _v ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
        | Tokens.SYMBOL _v ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
        | Tokens.RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv621 * _menhir_state * 'tv_attribute_value) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, x) = _menhir_stack in
            let _v : 'tv_nonempty_list_attribute_value_ = 
# 124 "/home/egoergo/.opam/4.00.1/lib/menhir/standard.mly"
    ( [ x ] )
# 942 "smtlib/parser.ml"
             in
            _menhir_goto_nonempty_list_attribute_value_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv622)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState84) : 'freshtv624)) : 'freshtv626)
    | MenhirState78 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv629 * _menhir_state * (
# 12 "smtlib/parser.mly"
      (string)
# 954 "smtlib/parser.ml"
        )) * _menhir_state * 'tv_attribute_value) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv627 * _menhir_state * (
# 12 "smtlib/parser.mly"
      (string)
# 960 "smtlib/parser.ml"
        )) * _menhir_state * 'tv_attribute_value) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, k0), _, a) = _menhir_stack in
        let _v : 'tv_attribute = let k =
          let k = k0 in
          
# 73 "smtlib/parser.mly"
                           ( Keyword k )
# 968 "smtlib/parser.ml"
          
        in
        
# 106 "smtlib/parser.mly"
                              ( (k, Some a) )
# 974 "smtlib/parser.ml"
         in
        _menhir_goto_attribute _menhir_env _menhir_stack _menhir_s _v) : 'freshtv628)) : 'freshtv630)
    | _ ->
        _menhir_fail ()

and _menhir_goto_expression : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_expression -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState45 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv549 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 989 "smtlib/parser.ml"
        )) * _menhir_state * 'tv_expression) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv547 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 997 "smtlib/parser.ml"
        )) * _menhir_state * 'tv_expression) = _menhir_stack in
        let (_tok : Tokens.token) = _tok in
        ((match _tok with
        | Tokens.RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv543 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 1006 "smtlib/parser.ml"
            )) * _menhir_state * 'tv_expression) = Obj.magic _menhir_stack in
            ((let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv541 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 1013 "smtlib/parser.ml"
            )) * _menhir_state * 'tv_expression) = _menhir_stack in
            let (_tok : Tokens.token) = _tok in
            ((match _tok with
            | Tokens.LPAREN ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState52
            | Tokens.RPAREN ->
                _menhir_reduce8 _menhir_env (Obj.magic _menhir_stack) MenhirState52
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState52) : 'freshtv542)) : 'freshtv544)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv545 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 1032 "smtlib/parser.ml"
            )) * _menhir_state * 'tv_expression) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv546)) : 'freshtv548)) : 'freshtv550)
    | MenhirState56 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv559 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_bind_list) * _menhir_state * 'tv_expression) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv557 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_bind_list) * _menhir_state * 'tv_expression) = _menhir_stack in
        let (_tok : Tokens.token) = _tok in
        ((match _tok with
        | Tokens.RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv553 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_bind_list) * _menhir_state * 'tv_expression) = Obj.magic _menhir_stack in
            ((let _ = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv551 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_bind_list) * _menhir_state * 'tv_expression) = Obj.magic _menhir_stack in
            ((let ((((_menhir_stack, _menhir_s), _), _, bs), _, e) = _menhir_stack in
            let _v : 'tv_expression = 
# 127 "smtlib/parser.mly"
                                                            ( Let (bs,e) )
# 1055 "smtlib/parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v) : 'freshtv552)) : 'freshtv554)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv555 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_bind_list) * _menhir_state * 'tv_expression) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv556)) : 'freshtv558)) : 'freshtv560)
    | MenhirState67 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv569 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_arg_list) * _menhir_state * 'tv_expression) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv567 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_arg_list) * _menhir_state * 'tv_expression) = _menhir_stack in
        let (_tok : Tokens.token) = _tok in
        ((match _tok with
        | Tokens.RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv563 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_arg_list) * _menhir_state * 'tv_expression) = Obj.magic _menhir_stack in
            ((let _ = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv561 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_arg_list) * _menhir_state * 'tv_expression) = Obj.magic _menhir_stack in
            ((let ((((_menhir_stack, _menhir_s), _), _, sys), _, e) = _menhir_stack in
            let _v : 'tv_expression = 
# 125 "smtlib/parser.mly"
                                                               ( Forall (sys,e) )
# 1084 "smtlib/parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v) : 'freshtv562)) : 'freshtv564)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv565 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_arg_list) * _menhir_state * 'tv_expression) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv566)) : 'freshtv568)) : 'freshtv570)
    | MenhirState73 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv579 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_arg_list) * _menhir_state * 'tv_expression) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv577 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_arg_list) * _menhir_state * 'tv_expression) = _menhir_stack in
        let (_tok : Tokens.token) = _tok in
        ((match _tok with
        | Tokens.RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv573 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_arg_list) * _menhir_state * 'tv_expression) = Obj.magic _menhir_stack in
            ((let _ = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv571 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_arg_list) * _menhir_state * 'tv_expression) = Obj.magic _menhir_stack in
            ((let ((((_menhir_stack, _menhir_s), _), _, sys), _, e) = _menhir_stack in
            let _v : 'tv_expression = 
# 126 "smtlib/parser.mly"
                                                               ( Exists (sys,e) )
# 1113 "smtlib/parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v) : 'freshtv572)) : 'freshtv574)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv575 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_arg_list) * _menhir_state * 'tv_expression) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv576)) : 'freshtv578)) : 'freshtv580)
    | MenhirState76 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv583 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_expression) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv581 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_expression) = _menhir_stack in
        let (_tok : Tokens.token) = _tok in
        ((match _tok with
        | Tokens.KEYWORD _v ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState77) : 'freshtv582)) : 'freshtv584)
    | MenhirState18 | MenhirState94 | MenhirState91 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv589 * _menhir_state * 'tv_expression) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv587 * _menhir_state * 'tv_expression) = _menhir_stack in
        let (_tok : Tokens.token) = _tok in
        ((match _tok with
        | Tokens.BIN _v ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
        | Tokens.DEC _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
        | Tokens.HEX _v ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
        | Tokens.LPAREN ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | Tokens.NUM _v ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
        | Tokens.STR _v ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
        | Tokens.SYMBOL _v ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
        | Tokens.RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv585 * _menhir_state * 'tv_expression) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, x) = _menhir_stack in
            let _v : 'tv_nonempty_list_expression_ = 
# 124 "/home/egoergo/.opam/4.00.1/lib/menhir/standard.mly"
    ( [ x ] )
# 1168 "smtlib/parser.ml"
             in
            _menhir_goto_nonempty_list_expression_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv586)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState94) : 'freshtv588)) : 'freshtv590)
    | MenhirState120 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv599 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 1180 "smtlib/parser.ml"
        )) * _menhir_state * 'tv_nonempty_list_symbol_) * _menhir_state * 'tv_expression) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv597 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 1188 "smtlib/parser.ml"
        )) * _menhir_state * 'tv_nonempty_list_symbol_) * _menhir_state * 'tv_expression) = _menhir_stack in
        let (_tok : Tokens.token) = _tok in
        ((match _tok with
        | Tokens.RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv593 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 1197 "smtlib/parser.ml"
            )) * _menhir_state * 'tv_nonempty_list_symbol_) * _menhir_state * 'tv_expression) = Obj.magic _menhir_stack in
            ((let _ = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv591 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 1204 "smtlib/parser.ml"
            )) * _menhir_state * 'tv_nonempty_list_symbol_) * _menhir_state * 'tv_expression) = Obj.magic _menhir_stack in
            ((let ((((_menhir_stack, _menhir_s), s0), _, syms), _, expr) = _menhir_stack in
            let _v : 'tv_command = let sym =
              let s = s0 in
              
# 72 "smtlib/parser.mly"
                         ( Symbol s )
# 1212 "smtlib/parser.ml"
              
            in
            
# 52 "smtlib/parser.mly"
                                                                                               (
    DefineSort (sym,syms,expr)
  )
# 1220 "smtlib/parser.ml"
             in
            _menhir_goto_command _menhir_env _menhir_stack _menhir_s _v) : 'freshtv592)) : 'freshtv594)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv595 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 1230 "smtlib/parser.ml"
            )) * _menhir_state * 'tv_nonempty_list_symbol_) * _menhir_state * 'tv_expression) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv596)) : 'freshtv598)) : 'freshtv600)
    | MenhirState128 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv609 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 1239 "smtlib/parser.ml"
        )) * _menhir_state * 'tv_arg_list) * _menhir_state * 'tv_sort) * _menhir_state * 'tv_expression) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv607 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 1247 "smtlib/parser.ml"
        )) * _menhir_state * 'tv_arg_list) * _menhir_state * 'tv_sort) * _menhir_state * 'tv_expression) = _menhir_stack in
        let (_tok : Tokens.token) = _tok in
        ((match _tok with
        | Tokens.RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv603 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 1256 "smtlib/parser.ml"
            )) * _menhir_state * 'tv_arg_list) * _menhir_state * 'tv_sort) * _menhir_state * 'tv_expression) = Obj.magic _menhir_stack in
            ((let _ = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv601 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 1263 "smtlib/parser.ml"
            )) * _menhir_state * 'tv_arg_list) * _menhir_state * 'tv_sort) * _menhir_state * 'tv_expression) = Obj.magic _menhir_stack in
            ((let (((((_menhir_stack, _menhir_s), s0), _, args), _, sort), _, expr) = _menhir_stack in
            let _v : 'tv_command = let sym =
              let s = s0 in
              
# 72 "smtlib/parser.mly"
                         ( Symbol s )
# 1271 "smtlib/parser.ml"
              
            in
            
# 46 "smtlib/parser.mly"
                                                                                           (
    DefineFun (sym,args,sort,expr)
  )
# 1279 "smtlib/parser.ml"
             in
            _menhir_goto_command _menhir_env _menhir_stack _menhir_s _v) : 'freshtv602)) : 'freshtv604)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv605 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 1289 "smtlib/parser.ml"
            )) * _menhir_state * 'tv_arg_list) * _menhir_state * 'tv_sort) * _menhir_state * 'tv_expression) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv606)) : 'freshtv608)) : 'freshtv610)
    | MenhirState146 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv619 * _menhir_state) * _menhir_state * 'tv_expression) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv617 * _menhir_state) * _menhir_state * 'tv_expression) = _menhir_stack in
        let (_tok : Tokens.token) = _tok in
        ((match _tok with
        | Tokens.RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv613 * _menhir_state) * _menhir_state * 'tv_expression) = Obj.magic _menhir_stack in
            ((let _ = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv611 * _menhir_state) * _menhir_state * 'tv_expression) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, expr) = _menhir_stack in
            let _v : 'tv_command = 
# 55 "smtlib/parser.mly"
                                       (
    Assert expr
  )
# 1314 "smtlib/parser.ml"
             in
            _menhir_goto_command _menhir_env _menhir_stack _menhir_s _v) : 'freshtv612)) : 'freshtv614)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv615 * _menhir_state) * _menhir_state * 'tv_expression) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv616)) : 'freshtv618)) : 'freshtv620)
    | _ ->
        _menhir_fail ()

and _menhir_goto_nonempty_list_symbol_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_nonempty_list_symbol_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState117 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv529 * _menhir_state * (
# 12 "smtlib/parser.mly"
      (string)
# 1336 "smtlib/parser.ml"
        )) * _menhir_state * 'tv_nonempty_list_symbol_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv527 * _menhir_state * (
# 12 "smtlib/parser.mly"
      (string)
# 1342 "smtlib/parser.ml"
        )) * _menhir_state * 'tv_nonempty_list_symbol_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, s0), _, xs) = _menhir_stack in
        let _v : 'tv_nonempty_list_symbol_ = let x =
          let s = s0 in
          
# 72 "smtlib/parser.mly"
                         ( Symbol s )
# 1350 "smtlib/parser.ml"
          
        in
        
# 126 "/home/egoergo/.opam/4.00.1/lib/menhir/standard.mly"
    ( x :: xs )
# 1356 "smtlib/parser.ml"
         in
        _menhir_goto_nonempty_list_symbol_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv528)) : 'freshtv530)
    | MenhirState116 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv539 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 1364 "smtlib/parser.ml"
        )) * _menhir_state * 'tv_nonempty_list_symbol_) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv537 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 1372 "smtlib/parser.ml"
        )) * _menhir_state * 'tv_nonempty_list_symbol_) = _menhir_stack in
        let (_tok : Tokens.token) = _tok in
        ((match _tok with
        | Tokens.RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv533 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 1381 "smtlib/parser.ml"
            )) * _menhir_state * 'tv_nonempty_list_symbol_) = Obj.magic _menhir_stack in
            ((let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv531 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 1388 "smtlib/parser.ml"
            )) * _menhir_state * 'tv_nonempty_list_symbol_) = _menhir_stack in
            let (_tok : Tokens.token) = _tok in
            ((match _tok with
            | Tokens.BIN _v ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v
            | Tokens.DEC _v ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v
            | Tokens.HEX _v ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v
            | Tokens.LPAREN ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState120
            | Tokens.NUM _v ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v
            | Tokens.STR _v ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v
            | Tokens.SYMBOL _v ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState120) : 'freshtv532)) : 'freshtv534)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv535 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 1417 "smtlib/parser.ml"
            )) * _menhir_state * 'tv_nonempty_list_symbol_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv536)) : 'freshtv538)) : 'freshtv540)
    | _ ->
        _menhir_fail ()

and _menhir_goto_arg_list : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_arg_list -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState64 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv495 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 1433 "smtlib/parser.ml"
        )) * _menhir_state * 'tv_sort) * _menhir_state * 'tv_arg_list) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv493 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 1439 "smtlib/parser.ml"
        )) * _menhir_state * 'tv_sort) * _menhir_state * 'tv_arg_list) = Obj.magic _menhir_stack in
        ((let ((((_menhir_stack, _menhir_s), s0), _, st), _, ss) = _menhir_stack in
        let _v : 'tv_arg_list = let sy =
          let s = s0 in
          
# 72 "smtlib/parser.mly"
                         ( Symbol s )
# 1447 "smtlib/parser.ml"
          
        in
        
# 83 "smtlib/parser.mly"
                                              (
    (sy,st)::ss
  )
# 1455 "smtlib/parser.ml"
         in
        _menhir_goto_arg_list _menhir_env _menhir_stack _menhir_s _v) : 'freshtv494)) : 'freshtv496)
    | MenhirState60 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv505 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_arg_list) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv503 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_arg_list) = _menhir_stack in
        let (_tok : Tokens.token) = _tok in
        ((match _tok with
        | Tokens.RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv499 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_arg_list) = Obj.magic _menhir_stack in
            ((let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv497 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_arg_list) = _menhir_stack in
            let (_tok : Tokens.token) = _tok in
            ((match _tok with
            | Tokens.BIN _v ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
            | Tokens.DEC _v ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
            | Tokens.HEX _v ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
            | Tokens.LPAREN ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | Tokens.NUM _v ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
            | Tokens.STR _v ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
            | Tokens.SYMBOL _v ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState67) : 'freshtv498)) : 'freshtv500)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv501 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_arg_list) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv502)) : 'freshtv504)) : 'freshtv506)
    | MenhirState71 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv515 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_arg_list) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv513 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_arg_list) = _menhir_stack in
        let (_tok : Tokens.token) = _tok in
        ((match _tok with
        | Tokens.RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv509 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_arg_list) = Obj.magic _menhir_stack in
            ((let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv507 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_arg_list) = _menhir_stack in
            let (_tok : Tokens.token) = _tok in
            ((match _tok with
            | Tokens.BIN _v ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
            | Tokens.DEC _v ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
            | Tokens.HEX _v ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
            | Tokens.LPAREN ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | Tokens.NUM _v ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
            | Tokens.STR _v ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
            | Tokens.SYMBOL _v ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState73) : 'freshtv508)) : 'freshtv510)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv511 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_arg_list) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv512)) : 'freshtv514)) : 'freshtv516)
    | MenhirState125 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv525 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 1547 "smtlib/parser.ml"
        )) * _menhir_state * 'tv_arg_list) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv523 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 1555 "smtlib/parser.ml"
        )) * _menhir_state * 'tv_arg_list) = _menhir_stack in
        let (_tok : Tokens.token) = _tok in
        ((match _tok with
        | Tokens.RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv519 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 1564 "smtlib/parser.ml"
            )) * _menhir_state * 'tv_arg_list) = Obj.magic _menhir_stack in
            ((let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv517 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 1571 "smtlib/parser.ml"
            )) * _menhir_state * 'tv_arg_list) = _menhir_stack in
            let (_tok : Tokens.token) = _tok in
            ((match _tok with
            | Tokens.LPAREN ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState127
            | Tokens.SYMBOL _v ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState127) : 'freshtv518)) : 'freshtv520)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv521 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 1590 "smtlib/parser.ml"
            )) * _menhir_state * 'tv_arg_list) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv522)) : 'freshtv524)) : 'freshtv526)
    | _ ->
        _menhir_fail ()

and _menhir_goto_sort_list : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_sort_list -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState137 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv487 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 1606 "smtlib/parser.ml"
        )) * _menhir_state * 'tv_sort_list) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv485 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 1614 "smtlib/parser.ml"
        )) * _menhir_state * 'tv_sort_list) = _menhir_stack in
        let (_tok : Tokens.token) = _tok in
        ((match _tok with
        | Tokens.RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv481 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 1623 "smtlib/parser.ml"
            )) * _menhir_state * 'tv_sort_list) = Obj.magic _menhir_stack in
            ((let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv479 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 1630 "smtlib/parser.ml"
            )) * _menhir_state * 'tv_sort_list) = _menhir_stack in
            let (_tok : Tokens.token) = _tok in
            ((match _tok with
            | Tokens.LPAREN ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState139
            | Tokens.SYMBOL _v ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState139 _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState139) : 'freshtv480)) : 'freshtv482)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv483 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 1649 "smtlib/parser.ml"
            )) * _menhir_state * 'tv_sort_list) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv484)) : 'freshtv486)) : 'freshtv488)
    | MenhirState142 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv491 * _menhir_state * 'tv_sort) * _menhir_state * 'tv_sort_list) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv489 * _menhir_state * 'tv_sort) * _menhir_state * 'tv_sort_list) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, s), _, ss) = _menhir_stack in
        let _v : 'tv_sort_list = 
# 77 "smtlib/parser.mly"
                      (
    s::ss
  )
# 1664 "smtlib/parser.ml"
         in
        _menhir_goto_sort_list _menhir_env _menhir_stack _menhir_s _v) : 'freshtv490)) : 'freshtv492)
    | _ ->
        _menhir_fail ()

and _menhir_run31 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv477 * _menhir_state) = _menhir_stack in
    let (_tok : Tokens.token) = _tok in
    ((match _tok with
    | Tokens.UNDERSCORE ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState31) : 'freshtv478)

and _menhir_goto_nonempty_list_command_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_nonempty_list_command_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv471 * _menhir_state * 'tv_nonempty_list_command_) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv469 * _menhir_state * 'tv_nonempty_list_command_) = _menhir_stack in
        let (_tok : Tokens.token) = _tok in
        ((match _tok with
        | Tokens.EOF ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv465 * _menhir_state * 'tv_nonempty_list_command_) = Obj.magic _menhir_stack in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv463 * _menhir_state * 'tv_nonempty_list_command_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, cs) = _menhir_stack in
            let _v : (
# 28 "smtlib/parser.mly"
      (Syntax.command list)
# 1707 "smtlib/parser.ml"
            ) = 
# 32 "smtlib/parser.mly"
                                         (
    cs
  )
# 1713 "smtlib/parser.ml"
             in
            _menhir_goto_program _menhir_env _menhir_stack _menhir_s _v) : 'freshtv464)) : 'freshtv466)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv467 * _menhir_state * 'tv_nonempty_list_command_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv468)) : 'freshtv470)) : 'freshtv472)
    | MenhirState152 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv475 * _menhir_state * 'tv_command) * _menhir_state * 'tv_nonempty_list_command_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv473 * _menhir_state * 'tv_command) * _menhir_state * 'tv_nonempty_list_command_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, x), _, xs) = _menhir_stack in
        let _v : 'tv_nonempty_list_command_ = 
# 126 "/home/egoergo/.opam/4.00.1/lib/menhir/standard.mly"
    ( x :: xs )
# 1732 "smtlib/parser.ml"
         in
        _menhir_goto_nonempty_list_command_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv474)) : 'freshtv476)
    | _ ->
        _menhir_fail ()

and _menhir_goto_identifier : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_identifier -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState30 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv449 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_identifier) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv447 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_identifier) = _menhir_stack in
        let (_tok : Tokens.token) = _tok in
        ((match _tok with
        | Tokens.LPAREN ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState32
        | Tokens.SYMBOL _v ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState32) : 'freshtv448)) : 'freshtv450)
    | MenhirState33 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv453 * _menhir_state) * _menhir_state * 'tv_identifier) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv451 * _menhir_state) * _menhir_state * 'tv_identifier) = _menhir_stack in
        let (_tok : Tokens.token) = _tok in
        ((match _tok with
        | Tokens.LPAREN ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | Tokens.SYMBOL _v ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState34) : 'freshtv452)) : 'freshtv454)
    | MenhirState137 | MenhirState142 | MenhirState139 | MenhirState127 | MenhirState62 | MenhirState32 | MenhirState34 | MenhirState35 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv457 * _menhir_state * 'tv_identifier) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv455 * _menhir_state * 'tv_identifier) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, id) = _menhir_stack in
        let _v : 'tv_sort = 
# 102 "smtlib/parser.mly"
                ( Sort (id, []) )
# 1785 "smtlib/parser.ml"
         in
        _menhir_goto_sort _menhir_env _menhir_stack _menhir_s _v) : 'freshtv456)) : 'freshtv458)
    | MenhirState146 | MenhirState128 | MenhirState120 | MenhirState18 | MenhirState22 | MenhirState94 | MenhirState91 | MenhirState76 | MenhirState73 | MenhirState67 | MenhirState56 | MenhirState45 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv461 * _menhir_state * 'tv_identifier) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv459 * _menhir_state * 'tv_identifier) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, id) = _menhir_stack in
        let _v : 'tv_qidentifier = 
# 99 "smtlib/parser.mly"
                ( NonQualified id )
# 1797 "smtlib/parser.ml"
         in
        _menhir_goto_qidentifier _menhir_env _menhir_stack _menhir_s _v) : 'freshtv460)) : 'freshtv462)
    | _ ->
        _menhir_fail ()

and _menhir_run23 : _menhir_env -> 'ttv_tail * _menhir_state -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv445 * _menhir_state) * _menhir_state) = _menhir_stack in
    let (_tok : Tokens.token) = _tok in
    ((match _tok with
    | Tokens.SYMBOL _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv441 * _menhir_state) * _menhir_state) = Obj.magic _menhir_stack in
        let (_v : (
# 12 "smtlib/parser.mly"
      (string)
# 1817 "smtlib/parser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _tok = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv439 * _menhir_state) * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 1825 "smtlib/parser.ml"
        )) = _menhir_stack in
        let (_tok : Tokens.token) = _tok in
        ((match _tok with
        | Tokens.NUM _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState24) : 'freshtv440)) : 'freshtv442)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv443 * _menhir_state) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv444)) : 'freshtv446)

and _menhir_reduce8 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_bind_list = 
# 88 "smtlib/parser.mly"
            ( [] )
# 1848 "smtlib/parser.ml"
     in
    _menhir_goto_bind_list _menhir_env _menhir_stack _menhir_s _v

and _menhir_run44 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv437 * _menhir_state) = _menhir_stack in
    let (_tok : Tokens.token) = _tok in
    ((match _tok with
    | Tokens.SYMBOL _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv433 * _menhir_state) = Obj.magic _menhir_stack in
        let (_v : (
# 12 "smtlib/parser.mly"
      (string)
# 1866 "smtlib/parser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _tok = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv431 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 1874 "smtlib/parser.ml"
        )) = _menhir_stack in
        let (_tok : Tokens.token) = _tok in
        ((match _tok with
        | Tokens.BIN _v ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
        | Tokens.DEC _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
        | Tokens.HEX _v ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
        | Tokens.LPAREN ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | Tokens.NUM _v ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
        | Tokens.STR _v ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
        | Tokens.SYMBOL _v ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState45) : 'freshtv432)) : 'freshtv434)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv435 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv436)) : 'freshtv438)

and _menhir_run30 : _menhir_env -> 'ttv_tail * _menhir_state -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv429 * _menhir_state) * _menhir_state) = _menhir_stack in
    let (_tok : Tokens.token) = _tok in
    ((match _tok with
    | Tokens.LPAREN ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | Tokens.SYMBOL _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState30) : 'freshtv430)

and _menhir_goto_constant : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_constant -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState146 | MenhirState128 | MenhirState120 | MenhirState18 | MenhirState91 | MenhirState94 | MenhirState76 | MenhirState73 | MenhirState67 | MenhirState56 | MenhirState45 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv423) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_constant) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv421) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (c : 'tv_constant) = _v in
        ((let _v : 'tv_expression = 
# 122 "smtlib/parser.mly"
             ( Prim c )
# 1936 "smtlib/parser.ml"
         in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v) : 'freshtv422)) : 'freshtv424)
    | MenhirState78 | MenhirState84 | MenhirState80 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv427) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_constant) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv425) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (c : 'tv_constant) = _v in
        ((let _v : 'tv_attribute_value = 
# 110 "smtlib/parser.mly"
             ( VPrim c )
# 1951 "smtlib/parser.ml"
         in
        _menhir_goto_attribute_value _menhir_env _menhir_stack _menhir_s _v) : 'freshtv426)) : 'freshtv428)
    | _ ->
        _menhir_fail ()

and _menhir_goto_program : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 28 "smtlib/parser.mly"
      (Syntax.command list)
# 1960 "smtlib/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv419) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : (
# 28 "smtlib/parser.mly"
      (Syntax.command list)
# 1969 "smtlib/parser.ml"
    )) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv417) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_1 : (
# 28 "smtlib/parser.mly"
      (Syntax.command list)
# 1977 "smtlib/parser.ml"
    )) = _v in
    (Obj.magic _1 : 'freshtv418)) : 'freshtv420)

and _menhir_run117 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 12 "smtlib/parser.mly"
      (string)
# 1984 "smtlib/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv415 * _menhir_state * (
# 12 "smtlib/parser.mly"
      (string)
# 1993 "smtlib/parser.ml"
    )) = _menhir_stack in
    let (_tok : Tokens.token) = _tok in
    ((match _tok with
    | Tokens.SYMBOL _v ->
        _menhir_run117 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _v
    | Tokens.RPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv413 * _menhir_state * (
# 12 "smtlib/parser.mly"
      (string)
# 2004 "smtlib/parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, s0) = _menhir_stack in
        let _v : 'tv_nonempty_list_symbol_ = let x =
          let s = s0 in
          
# 72 "smtlib/parser.mly"
                         ( Symbol s )
# 2012 "smtlib/parser.ml"
          
        in
        
# 124 "/home/egoergo/.opam/4.00.1/lib/menhir/standard.mly"
    ( [ x ] )
# 2018 "smtlib/parser.ml"
         in
        _menhir_goto_nonempty_list_symbol_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv414)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState117) : 'freshtv416)

and _menhir_reduce1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_arg_list = 
# 82 "smtlib/parser.mly"
            ( [] )
# 2031 "smtlib/parser.ml"
     in
    _menhir_goto_arg_list _menhir_env _menhir_stack _menhir_s _v

and _menhir_run61 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv411 * _menhir_state) = _menhir_stack in
    let (_tok : Tokens.token) = _tok in
    ((match _tok with
    | Tokens.SYMBOL _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv407 * _menhir_state) = Obj.magic _menhir_stack in
        let (_v : (
# 12 "smtlib/parser.mly"
      (string)
# 2049 "smtlib/parser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _tok = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv405 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 2057 "smtlib/parser.ml"
        )) = _menhir_stack in
        let (_tok : Tokens.token) = _tok in
        ((match _tok with
        | Tokens.LPAREN ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | Tokens.SYMBOL _v ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState62) : 'freshtv406)) : 'freshtv408)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv409 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv410)) : 'freshtv412)

and _menhir_reduce63 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_sort_list = 
# 76 "smtlib/parser.mly"
            ( [] )
# 2082 "smtlib/parser.ml"
     in
    _menhir_goto_sort_list _menhir_env _menhir_stack _menhir_s _v

and _menhir_run33 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv403 * _menhir_state) = _menhir_stack in
    let (_tok : Tokens.token) = _tok in
    ((match _tok with
    | Tokens.LPAREN ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | Tokens.SYMBOL _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
    | Tokens.UNDERSCORE ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState33) : 'freshtv404)

and _menhir_goto_command : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_command -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv401 * _menhir_state * 'tv_command) = Obj.magic _menhir_stack in
    ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv399 * _menhir_state * 'tv_command) = _menhir_stack in
    let (_tok : Tokens.token) = _tok in
    ((match _tok with
    | Tokens.LPAREN ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState152
    | Tokens.EOF ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv397 * _menhir_state * 'tv_command) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : 'tv_nonempty_list_command_ = 
# 124 "/home/egoergo/.opam/4.00.1/lib/menhir/standard.mly"
    ( [ x ] )
# 2125 "smtlib/parser.ml"
         in
        _menhir_goto_nonempty_list_command_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv398)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState152) : 'freshtv400)) : 'freshtv402)

and _menhir_run19 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 12 "smtlib/parser.mly"
      (string)
# 2136 "smtlib/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv395) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (s0 : (
# 12 "smtlib/parser.mly"
      (string)
# 2146 "smtlib/parser.ml"
    )) = _v in
    ((let _v : 'tv_identifier = let sym =
      let s = s0 in
      
# 72 "smtlib/parser.mly"
                         ( Symbol s )
# 2153 "smtlib/parser.ml"
      
    in
    
# 94 "smtlib/parser.mly"
             ( Simple sym )
# 2159 "smtlib/parser.ml"
     in
    _menhir_goto_identifier _menhir_env _menhir_stack _menhir_s _v) : 'freshtv396)

and _menhir_run20 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 16 "smtlib/parser.mly"
      (string)
# 2166 "smtlib/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv393) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (s : (
# 16 "smtlib/parser.mly"
      (string)
# 2176 "smtlib/parser.ml"
    )) = _v in
    ((let _v : 'tv_constant = 
# 119 "smtlib/parser.mly"
        ( Str s )
# 2181 "smtlib/parser.ml"
     in
    _menhir_goto_constant _menhir_env _menhir_stack _menhir_s _v) : 'freshtv394)

and _menhir_run21 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 15 "smtlib/parser.mly"
      (int)
# 2188 "smtlib/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv391) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (n : (
# 15 "smtlib/parser.mly"
      (int)
# 2198 "smtlib/parser.ml"
    )) = _v in
    ((let _v : 'tv_constant = 
# 115 "smtlib/parser.mly"
        ( Num n )
# 2203 "smtlib/parser.ml"
     in
    _menhir_goto_constant _menhir_env _menhir_stack _menhir_s _v) : 'freshtv392)

and _menhir_run22 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv389 * _menhir_state) = _menhir_stack in
    let (_tok : Tokens.token) = _tok in
    ((match _tok with
    | Tokens.AS ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | Tokens.BANG ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv353 * _menhir_state) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState22 in
        ((let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _tok = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv351 * _menhir_state) * _menhir_state) = _menhir_stack in
        let (_tok : Tokens.token) = _tok in
        ((match _tok with
        | Tokens.BIN _v ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
        | Tokens.DEC _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
        | Tokens.HEX _v ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
        | Tokens.LPAREN ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | Tokens.NUM _v ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
        | Tokens.STR _v ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
        | Tokens.SYMBOL _v ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState76) : 'freshtv352)) : 'freshtv354)
    | Tokens.EXISTS ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv363 * _menhir_state) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState22 in
        ((let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _tok = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv361 * _menhir_state) * _menhir_state) = _menhir_stack in
        let (_tok : Tokens.token) = _tok in
        ((match _tok with
        | Tokens.LPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv357 * _menhir_state) * _menhir_state) = Obj.magic _menhir_stack in
            ((let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv355 * _menhir_state) * _menhir_state) = _menhir_stack in
            let (_tok : Tokens.token) = _tok in
            ((match _tok with
            | Tokens.LPAREN ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState71
            | Tokens.RPAREN ->
                _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack) MenhirState71
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState71) : 'freshtv356)) : 'freshtv358)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv359 * _menhir_state) * _menhir_state) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv360)) : 'freshtv362)) : 'freshtv364)
    | Tokens.FORALL ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv373 * _menhir_state) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState22 in
        ((let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _tok = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv371 * _menhir_state) * _menhir_state) = _menhir_stack in
        let (_tok : Tokens.token) = _tok in
        ((match _tok with
        | Tokens.LPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv367 * _menhir_state) * _menhir_state) = Obj.magic _menhir_stack in
            ((let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv365 * _menhir_state) * _menhir_state) = _menhir_stack in
            let (_tok : Tokens.token) = _tok in
            ((match _tok with
            | Tokens.LPAREN ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState60
            | Tokens.RPAREN ->
                _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack) MenhirState60
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState60) : 'freshtv366)) : 'freshtv368)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv369 * _menhir_state) * _menhir_state) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv370)) : 'freshtv372)) : 'freshtv374)
    | Tokens.LET ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv383 * _menhir_state) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState22 in
        ((let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _tok = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv381 * _menhir_state) * _menhir_state) = _menhir_stack in
        let (_tok : Tokens.token) = _tok in
        ((match _tok with
        | Tokens.LPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv377 * _menhir_state) * _menhir_state) = Obj.magic _menhir_stack in
            ((let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv375 * _menhir_state) * _menhir_state) = _menhir_stack in
            let (_tok : Tokens.token) = _tok in
            ((match _tok with
            | Tokens.LPAREN ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState43
            | Tokens.RPAREN ->
                _menhir_reduce8 _menhir_env (Obj.magic _menhir_stack) MenhirState43
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState43) : 'freshtv376)) : 'freshtv378)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv379 * _menhir_state) * _menhir_state) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv380)) : 'freshtv382)) : 'freshtv384)
    | Tokens.LPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv387 * _menhir_state) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState22 in
        ((let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _tok = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv385 * _menhir_state) * _menhir_state) = _menhir_stack in
        let (_tok : Tokens.token) = _tok in
        ((match _tok with
        | Tokens.AS ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState29
        | Tokens.UNDERSCORE ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState29
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState29) : 'freshtv386)) : 'freshtv388)
    | Tokens.SYMBOL _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
    | Tokens.UNDERSCORE ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState22) : 'freshtv390)

and _menhir_run46 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 16 "smtlib/parser.mly"
      (string)
# 2374 "smtlib/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv349) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (h : (
# 16 "smtlib/parser.mly"
      (string)
# 2384 "smtlib/parser.ml"
    )) = _v in
    ((let _v : 'tv_constant = 
# 118 "smtlib/parser.mly"
        ( Hex h )
# 2389 "smtlib/parser.ml"
     in
    _menhir_goto_constant _menhir_env _menhir_stack _menhir_s _v) : 'freshtv350)

and _menhir_run47 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 16 "smtlib/parser.mly"
      (string)
# 2396 "smtlib/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv347) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (d : (
# 16 "smtlib/parser.mly"
      (string)
# 2406 "smtlib/parser.ml"
    )) = _v in
    ((let _v : 'tv_constant = 
# 116 "smtlib/parser.mly"
        ( Dec d )
# 2411 "smtlib/parser.ml"
     in
    _menhir_goto_constant _menhir_env _menhir_stack _menhir_s _v) : 'freshtv348)

and _menhir_run48 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 16 "smtlib/parser.mly"
      (string)
# 2418 "smtlib/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv345) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (b : (
# 16 "smtlib/parser.mly"
      (string)
# 2428 "smtlib/parser.ml"
    )) = _v in
    ((let _v : 'tv_constant = 
# 117 "smtlib/parser.mly"
        ( Bin b )
# 2433 "smtlib/parser.ml"
     in
    _menhir_goto_constant _menhir_env _menhir_stack _menhir_s _v) : 'freshtv346)

and _menhir_discard : _menhir_env -> Tokens.token =
  fun _menhir_env ->
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = _menhir_env._menhir_lexer lexbuf in
    _menhir_env._menhir_token <- _tok;
    _menhir_env._menhir_startp <- lexbuf.Lexing.lex_start_p;
    _menhir_env._menhir_endp <- lexbuf.Lexing.lex_curr_p;
    let shifted = Pervasives.(+) _menhir_env._menhir_shifted 1 in
    if Pervasives.(>=) shifted 0 then
      _menhir_env._menhir_shifted <- shifted;
    _tok

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState152 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv259 * _menhir_state * 'tv_command) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv260)
    | MenhirState146 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv261 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv262)
    | MenhirState142 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv263 * _menhir_state * 'tv_sort) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv264)
    | MenhirState139 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv265 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 2472 "smtlib/parser.ml"
        )) * _menhir_state * 'tv_sort_list) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv266)
    | MenhirState137 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv267 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 2481 "smtlib/parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv268)
    | MenhirState128 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv269 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 2490 "smtlib/parser.ml"
        )) * _menhir_state * 'tv_arg_list) * _menhir_state * 'tv_sort) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv270)
    | MenhirState127 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv271 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 2499 "smtlib/parser.ml"
        )) * _menhir_state * 'tv_arg_list) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv272)
    | MenhirState125 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv273 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 2508 "smtlib/parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv274)
    | MenhirState120 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv275 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 2517 "smtlib/parser.ml"
        )) * _menhir_state * 'tv_nonempty_list_symbol_) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv276)
    | MenhirState117 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv277 * _menhir_state * (
# 12 "smtlib/parser.mly"
      (string)
# 2526 "smtlib/parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv278)
    | MenhirState116 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv279 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 2535 "smtlib/parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv280)
    | MenhirState94 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv281 * _menhir_state * 'tv_expression) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv282)
    | MenhirState91 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv283 * _menhir_state) * _menhir_state * 'tv_qidentifier) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv284)
    | MenhirState89 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv285 * _menhir_state * 'tv_attribute) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv286)
    | MenhirState84 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv287 * _menhir_state * 'tv_attribute_value) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv288)
    | MenhirState80 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv289 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv290)
    | MenhirState78 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv291 * _menhir_state * (
# 12 "smtlib/parser.mly"
      (string)
# 2569 "smtlib/parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv292)
    | MenhirState77 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv293 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_expression) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv294)
    | MenhirState76 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv295 * _menhir_state) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv296)
    | MenhirState73 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv297 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_arg_list) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv298)
    | MenhirState71 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv299 * _menhir_state) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv300)
    | MenhirState67 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv301 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_arg_list) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv302)
    | MenhirState64 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv303 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 2603 "smtlib/parser.ml"
        )) * _menhir_state * 'tv_sort) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv304)
    | MenhirState62 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv305 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 2612 "smtlib/parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv306)
    | MenhirState60 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv307 * _menhir_state) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv308)
    | MenhirState56 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv309 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_bind_list) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv310)
    | MenhirState52 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv311 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 2631 "smtlib/parser.ml"
        )) * _menhir_state * 'tv_expression) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv312)
    | MenhirState45 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv313 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 2640 "smtlib/parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv314)
    | MenhirState43 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv315 * _menhir_state) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv316)
    | MenhirState35 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv317 * _menhir_state * 'tv_sort) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv318)
    | MenhirState34 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv319 * _menhir_state) * _menhir_state * 'tv_identifier) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv320)
    | MenhirState33 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv321 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv322)
    | MenhirState32 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv323 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_identifier) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv324)
    | MenhirState31 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv325 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv326)
    | MenhirState30 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv327 * _menhir_state) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv328)
    | MenhirState29 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv329 * _menhir_state) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv330)
    | MenhirState25 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv331 * _menhir_state * (
# 15 "smtlib/parser.mly"
      (int)
# 2689 "smtlib/parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv332)
    | MenhirState24 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv333 * _menhir_state) * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 2698 "smtlib/parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv334)
    | MenhirState22 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv335 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv336)
    | MenhirState18 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv337 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv338)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv343) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv341) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState0 in
        let (_startpos : Lexing.position) = _menhir_env._menhir_startp in
        let (_endpos : Lexing.position) = _menhir_env._menhir_endp in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv339) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_startpos__1_ : Lexing.position) = _startpos in
        let (_endpos__1_ : Lexing.position) = _endpos in
        ((let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : (
# 28 "smtlib/parser.mly"
      (Syntax.command list)
# 2730 "smtlib/parser.ml"
        ) = 
# 35 "smtlib/parser.mly"
        (
    Errors.fatal [_startpos; _endpos] "Syntax error"
  )
# 2736 "smtlib/parser.ml"
         in
        _menhir_goto_program _menhir_env _menhir_stack _menhir_s _v) : 'freshtv340)) : 'freshtv342)) : 'freshtv344)

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv257 * _menhir_state) = _menhir_stack in
    let (_tok : Tokens.token) = _tok in
    ((match _tok with
    | Tokens.ASSERT ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv7 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _tok = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv5 * _menhir_state) = _menhir_stack in
        let (_tok : Tokens.token) = _tok in
        ((match _tok with
        | Tokens.BIN _v ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _v
        | Tokens.DEC _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _v
        | Tokens.HEX _v ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _v
        | Tokens.LPAREN ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState146
        | Tokens.NUM _v ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _v
        | Tokens.STR _v ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _v
        | Tokens.SYMBOL _v ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState146) : 'freshtv6)) : 'freshtv8)
    | Tokens.CHECKSAT ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv17 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _tok = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv15 * _menhir_state) = _menhir_stack in
        let (_tok : Tokens.token) = _tok in
        ((match _tok with
        | Tokens.RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv11 * _menhir_state) = Obj.magic _menhir_stack in
            ((let _ = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv9 * _menhir_state) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s) = _menhir_stack in
            let _v : 'tv_command = 
# 59 "smtlib/parser.mly"
                         ( CheckSat )
# 2792 "smtlib/parser.ml"
             in
            _menhir_goto_command _menhir_env _menhir_stack _menhir_s _v) : 'freshtv10)) : 'freshtv12)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv13 * _menhir_state) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv14)) : 'freshtv16)) : 'freshtv18)
    | Tokens.DECLAREFUN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv33 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _tok = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv31 * _menhir_state) = _menhir_stack in
        let (_tok : Tokens.token) = _tok in
        ((match _tok with
        | Tokens.SYMBOL _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv27 * _menhir_state) = Obj.magic _menhir_stack in
            let (_v : (
# 12 "smtlib/parser.mly"
      (string)
# 2816 "smtlib/parser.ml"
            )) = _v in
            ((let _menhir_stack = (_menhir_stack, _v) in
            let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv25 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 2824 "smtlib/parser.ml"
            )) = _menhir_stack in
            let (_tok : Tokens.token) = _tok in
            ((match _tok with
            | Tokens.LPAREN ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv21 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 2833 "smtlib/parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let _tok = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv19 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 2840 "smtlib/parser.ml"
                )) = _menhir_stack in
                let (_tok : Tokens.token) = _tok in
                ((match _tok with
                | Tokens.LPAREN ->
                    _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState137
                | Tokens.SYMBOL _v ->
                    _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _v
                | Tokens.RPAREN ->
                    _menhir_reduce63 _menhir_env (Obj.magic _menhir_stack) MenhirState137
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState137) : 'freshtv20)) : 'freshtv22)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv23 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 2861 "smtlib/parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv24)) : 'freshtv26)) : 'freshtv28)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv29 * _menhir_state) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv30)) : 'freshtv32)) : 'freshtv34)
    | Tokens.DECLARESORT ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv55 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _tok = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv53 * _menhir_state) = _menhir_stack in
        let (_tok : Tokens.token) = _tok in
        ((match _tok with
        | Tokens.SYMBOL _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv49 * _menhir_state) = Obj.magic _menhir_stack in
            let (_v : (
# 12 "smtlib/parser.mly"
      (string)
# 2886 "smtlib/parser.ml"
            )) = _v in
            ((let _menhir_stack = (_menhir_stack, _v) in
            let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv47 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 2894 "smtlib/parser.ml"
            )) = _menhir_stack in
            let (_tok : Tokens.token) = _tok in
            ((match _tok with
            | Tokens.NUM _v ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv43 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 2903 "smtlib/parser.ml"
                )) = Obj.magic _menhir_stack in
                let (_v : (
# 15 "smtlib/parser.mly"
      (int)
# 2908 "smtlib/parser.ml"
                )) = _v in
                ((let _menhir_stack = (_menhir_stack, _v) in
                let _tok = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv41 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 2916 "smtlib/parser.ml"
                )) * (
# 15 "smtlib/parser.mly"
      (int)
# 2920 "smtlib/parser.ml"
                )) = _menhir_stack in
                let (_tok : Tokens.token) = _tok in
                ((match _tok with
                | Tokens.RPAREN ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : (('freshtv37 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 2929 "smtlib/parser.ml"
                    )) * (
# 15 "smtlib/parser.mly"
      (int)
# 2933 "smtlib/parser.ml"
                    )) = Obj.magic _menhir_stack in
                    ((let _ = _menhir_discard _menhir_env in
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : (('freshtv35 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 2940 "smtlib/parser.ml"
                    )) * (
# 15 "smtlib/parser.mly"
      (int)
# 2944 "smtlib/parser.ml"
                    )) = Obj.magic _menhir_stack in
                    ((let (((_menhir_stack, _menhir_s), s0), n) = _menhir_stack in
                    let _v : 'tv_command = let sym =
                      let s = s0 in
                      
# 72 "smtlib/parser.mly"
                         ( Symbol s )
# 2952 "smtlib/parser.ml"
                      
                    in
                    
# 49 "smtlib/parser.mly"
                                             (
    DeclareSort (sym,n)
  )
# 2960 "smtlib/parser.ml"
                     in
                    _menhir_goto_command _menhir_env _menhir_stack _menhir_s _v) : 'freshtv36)) : 'freshtv38)
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : (('freshtv39 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 2970 "smtlib/parser.ml"
                    )) * (
# 15 "smtlib/parser.mly"
      (int)
# 2974 "smtlib/parser.ml"
                    )) = Obj.magic _menhir_stack in
                    ((let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv40)) : 'freshtv42)) : 'freshtv44)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv45 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 2985 "smtlib/parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv46)) : 'freshtv48)) : 'freshtv50)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv51 * _menhir_state) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv52)) : 'freshtv54)) : 'freshtv56)
    | Tokens.DEFINEFUN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv71 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _tok = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv69 * _menhir_state) = _menhir_stack in
        let (_tok : Tokens.token) = _tok in
        ((match _tok with
        | Tokens.SYMBOL _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv65 * _menhir_state) = Obj.magic _menhir_stack in
            let (_v : (
# 12 "smtlib/parser.mly"
      (string)
# 3010 "smtlib/parser.ml"
            )) = _v in
            ((let _menhir_stack = (_menhir_stack, _v) in
            let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv63 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 3018 "smtlib/parser.ml"
            )) = _menhir_stack in
            let (_tok : Tokens.token) = _tok in
            ((match _tok with
            | Tokens.LPAREN ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv59 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 3027 "smtlib/parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let _tok = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv57 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 3034 "smtlib/parser.ml"
                )) = _menhir_stack in
                let (_tok : Tokens.token) = _tok in
                ((match _tok with
                | Tokens.LPAREN ->
                    _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState125
                | Tokens.RPAREN ->
                    _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack) MenhirState125
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState125) : 'freshtv58)) : 'freshtv60)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv61 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 3053 "smtlib/parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv62)) : 'freshtv64)) : 'freshtv66)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv67 * _menhir_state) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv68)) : 'freshtv70)) : 'freshtv72)
    | Tokens.DEFINESORT ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv87 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _tok = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv85 * _menhir_state) = _menhir_stack in
        let (_tok : Tokens.token) = _tok in
        ((match _tok with
        | Tokens.SYMBOL _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv81 * _menhir_state) = Obj.magic _menhir_stack in
            let (_v : (
# 12 "smtlib/parser.mly"
      (string)
# 3078 "smtlib/parser.ml"
            )) = _v in
            ((let _menhir_stack = (_menhir_stack, _v) in
            let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv79 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 3086 "smtlib/parser.ml"
            )) = _menhir_stack in
            let (_tok : Tokens.token) = _tok in
            ((match _tok with
            | Tokens.LPAREN ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv75 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 3095 "smtlib/parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let _tok = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv73 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 3102 "smtlib/parser.ml"
                )) = _menhir_stack in
                let (_tok : Tokens.token) = _tok in
                ((match _tok with
                | Tokens.SYMBOL _v ->
                    _menhir_run117 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState116) : 'freshtv74)) : 'freshtv76)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv77 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 3119 "smtlib/parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv78)) : 'freshtv80)) : 'freshtv82)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv83 * _menhir_state) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv84)) : 'freshtv86)) : 'freshtv88)
    | Tokens.EXIT ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv97 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _tok = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv95 * _menhir_state) = _menhir_stack in
        let (_tok : Tokens.token) = _tok in
        ((match _tok with
        | Tokens.RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv91 * _menhir_state) = Obj.magic _menhir_stack in
            ((let _ = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv89 * _menhir_state) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s) = _menhir_stack in
            let _v : 'tv_command = 
# 70 "smtlib/parser.mly"
                     ( Exit )
# 3148 "smtlib/parser.ml"
             in
            _menhir_goto_command _menhir_env _menhir_stack _menhir_s _v) : 'freshtv90)) : 'freshtv92)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv93 * _menhir_state) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv94)) : 'freshtv96)) : 'freshtv98)
    | Tokens.GETASSERTIONS ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv107 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _tok = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv105 * _menhir_state) = _menhir_stack in
        let (_tok : Tokens.token) = _tok in
        ((match _tok with
        | Tokens.RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv101 * _menhir_state) = Obj.magic _menhir_stack in
            ((let _ = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv99 * _menhir_state) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s) = _menhir_stack in
            let _v : 'tv_command = 
# 58 "smtlib/parser.mly"
                              ( GetAssertions )
# 3176 "smtlib/parser.ml"
             in
            _menhir_goto_command _menhir_env _menhir_stack _menhir_s _v) : 'freshtv100)) : 'freshtv102)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv103 * _menhir_state) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv104)) : 'freshtv106)) : 'freshtv108)
    | Tokens.GETASSIGNMENT ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv117 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _tok = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv115 * _menhir_state) = _menhir_stack in
        let (_tok : Tokens.token) = _tok in
        ((match _tok with
        | Tokens.RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv111 * _menhir_state) = Obj.magic _menhir_stack in
            ((let _ = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv109 * _menhir_state) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s) = _menhir_stack in
            let _v : 'tv_command = 
# 63 "smtlib/parser.mly"
                              ( GetAssignment )
# 3204 "smtlib/parser.ml"
             in
            _menhir_goto_command _menhir_env _menhir_stack _menhir_s _v) : 'freshtv110)) : 'freshtv112)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv113 * _menhir_state) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv114)) : 'freshtv116)) : 'freshtv118)
    | Tokens.GETINFO ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv133 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _tok = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv131 * _menhir_state) = _menhir_stack in
        let (_tok : Tokens.token) = _tok in
        ((match _tok with
        | Tokens.KEYWORD _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv127 * _menhir_state) = Obj.magic _menhir_stack in
            let (_v : (
# 12 "smtlib/parser.mly"
      (string)
# 3228 "smtlib/parser.ml"
            )) = _v in
            ((let _menhir_stack = (_menhir_stack, _v) in
            let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv125 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 3236 "smtlib/parser.ml"
            )) = _menhir_stack in
            let (_tok : Tokens.token) = _tok in
            ((match _tok with
            | Tokens.RPAREN ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv121 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 3245 "smtlib/parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let _ = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv119 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 3252 "smtlib/parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s), k0) = _menhir_stack in
                let _v : 'tv_command = let k =
                  let k = k0 in
                  
# 73 "smtlib/parser.mly"
                           ( Keyword k )
# 3260 "smtlib/parser.ml"
                  
                in
                
# 68 "smtlib/parser.mly"
                                  ( GetInfo k )
# 3266 "smtlib/parser.ml"
                 in
                _menhir_goto_command _menhir_env _menhir_stack _menhir_s _v) : 'freshtv120)) : 'freshtv122)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv123 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 3276 "smtlib/parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv124)) : 'freshtv126)) : 'freshtv128)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv129 * _menhir_state) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv130)) : 'freshtv132)) : 'freshtv134)
    | Tokens.GETOPTION ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv149 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _tok = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv147 * _menhir_state) = _menhir_stack in
        let (_tok : Tokens.token) = _tok in
        ((match _tok with
        | Tokens.KEYWORD _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv143 * _menhir_state) = Obj.magic _menhir_stack in
            let (_v : (
# 12 "smtlib/parser.mly"
      (string)
# 3301 "smtlib/parser.ml"
            )) = _v in
            ((let _menhir_stack = (_menhir_stack, _v) in
            let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv141 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 3309 "smtlib/parser.ml"
            )) = _menhir_stack in
            let (_tok : Tokens.token) = _tok in
            ((match _tok with
            | Tokens.RPAREN ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv137 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 3318 "smtlib/parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let _ = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv135 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 3325 "smtlib/parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s), k0) = _menhir_stack in
                let _v : 'tv_command = let k =
                  let k = k0 in
                  
# 73 "smtlib/parser.mly"
                           ( Keyword k )
# 3333 "smtlib/parser.ml"
                  
                in
                
# 66 "smtlib/parser.mly"
                                    ( GetOption k )
# 3339 "smtlib/parser.ml"
                 in
                _menhir_goto_command _menhir_env _menhir_stack _menhir_s _v) : 'freshtv136)) : 'freshtv138)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv139 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 3349 "smtlib/parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv140)) : 'freshtv142)) : 'freshtv144)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv145 * _menhir_state) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv146)) : 'freshtv148)) : 'freshtv150)
    | Tokens.GETPROOF ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv159 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _tok = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv157 * _menhir_state) = _menhir_stack in
        let (_tok : Tokens.token) = _tok in
        ((match _tok with
        | Tokens.RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv153 * _menhir_state) = Obj.magic _menhir_stack in
            ((let _ = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv151 * _menhir_state) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s) = _menhir_stack in
            let _v : 'tv_command = 
# 60 "smtlib/parser.mly"
                         ( GetProof )
# 3378 "smtlib/parser.ml"
             in
            _menhir_goto_command _menhir_env _menhir_stack _menhir_s _v) : 'freshtv152)) : 'freshtv154)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv155 * _menhir_state) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv156)) : 'freshtv158)) : 'freshtv160)
    | Tokens.GETUNSATCORE ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv169 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _tok = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv167 * _menhir_state) = _menhir_stack in
        let (_tok : Tokens.token) = _tok in
        ((match _tok with
        | Tokens.RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv163 * _menhir_state) = Obj.magic _menhir_stack in
            ((let _ = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv161 * _menhir_state) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s) = _menhir_stack in
            let _v : 'tv_command = 
# 61 "smtlib/parser.mly"
                             ( GetUnsatCore )
# 3406 "smtlib/parser.ml"
             in
            _menhir_goto_command _menhir_env _menhir_stack _menhir_s _v) : 'freshtv162)) : 'freshtv164)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv165 * _menhir_state) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv166)) : 'freshtv168)) : 'freshtv170)
    | Tokens.GETVALUE ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv173 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _tok = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv171 * _menhir_state) = _menhir_stack in
        let (_tok : Tokens.token) = _tok in
        ((match _tok with
        | Tokens.BIN _v ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
        | Tokens.DEC _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
        | Tokens.HEX _v ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
        | Tokens.LPAREN ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | Tokens.NUM _v ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
        | Tokens.STR _v ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
        | Tokens.SYMBOL _v ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState18) : 'freshtv172)) : 'freshtv174)
    | Tokens.POP ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv189 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _tok = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv187 * _menhir_state) = _menhir_stack in
        let (_tok : Tokens.token) = _tok in
        ((match _tok with
        | Tokens.NUM _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv183 * _menhir_state) = Obj.magic _menhir_stack in
            let (_v : (
# 15 "smtlib/parser.mly"
      (int)
# 3456 "smtlib/parser.ml"
            )) = _v in
            ((let _menhir_stack = (_menhir_stack, _v) in
            let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv181 * _menhir_state) * (
# 15 "smtlib/parser.mly"
      (int)
# 3464 "smtlib/parser.ml"
            )) = _menhir_stack in
            let (_tok : Tokens.token) = _tok in
            ((match _tok with
            | Tokens.RPAREN ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv177 * _menhir_state) * (
# 15 "smtlib/parser.mly"
      (int)
# 3473 "smtlib/parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let _ = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv175 * _menhir_state) * (
# 15 "smtlib/parser.mly"
      (int)
# 3480 "smtlib/parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s), n) = _menhir_stack in
                let _v : 'tv_command = 
# 65 "smtlib/parser.mly"
                          ( Pop n )
# 3486 "smtlib/parser.ml"
                 in
                _menhir_goto_command _menhir_env _menhir_stack _menhir_s _v) : 'freshtv176)) : 'freshtv178)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv179 * _menhir_state) * (
# 15 "smtlib/parser.mly"
      (int)
# 3496 "smtlib/parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv180)) : 'freshtv182)) : 'freshtv184)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv185 * _menhir_state) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv186)) : 'freshtv188)) : 'freshtv190)
    | Tokens.PUSH ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv205 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _tok = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv203 * _menhir_state) = _menhir_stack in
        let (_tok : Tokens.token) = _tok in
        ((match _tok with
        | Tokens.NUM _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv199 * _menhir_state) = Obj.magic _menhir_stack in
            let (_v : (
# 15 "smtlib/parser.mly"
      (int)
# 3521 "smtlib/parser.ml"
            )) = _v in
            ((let _menhir_stack = (_menhir_stack, _v) in
            let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv197 * _menhir_state) * (
# 15 "smtlib/parser.mly"
      (int)
# 3529 "smtlib/parser.ml"
            )) = _menhir_stack in
            let (_tok : Tokens.token) = _tok in
            ((match _tok with
            | Tokens.RPAREN ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv193 * _menhir_state) * (
# 15 "smtlib/parser.mly"
      (int)
# 3538 "smtlib/parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let _ = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv191 * _menhir_state) * (
# 15 "smtlib/parser.mly"
      (int)
# 3545 "smtlib/parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s), n) = _menhir_stack in
                let _v : 'tv_command = 
# 64 "smtlib/parser.mly"
                           ( Push n )
# 3551 "smtlib/parser.ml"
                 in
                _menhir_goto_command _menhir_env _menhir_stack _menhir_s _v) : 'freshtv192)) : 'freshtv194)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv195 * _menhir_state) * (
# 15 "smtlib/parser.mly"
      (int)
# 3561 "smtlib/parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv196)) : 'freshtv198)) : 'freshtv200)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv201 * _menhir_state) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv202)) : 'freshtv204)) : 'freshtv206)
    | Tokens.SETINFO ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv221 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _tok = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv219 * _menhir_state) = _menhir_stack in
        let (_tok : Tokens.token) = _tok in
        ((match _tok with
        | Tokens.KEYWORD _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv215 * _menhir_state) = Obj.magic _menhir_stack in
            let (_v : (
# 12 "smtlib/parser.mly"
      (string)
# 3586 "smtlib/parser.ml"
            )) = _v in
            ((let _menhir_stack = (_menhir_stack, _v) in
            let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv213 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 3594 "smtlib/parser.ml"
            )) = _menhir_stack in
            let (_tok : Tokens.token) = _tok in
            ((match _tok with
            | Tokens.RPAREN ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv209 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 3603 "smtlib/parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let _ = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv207 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 3610 "smtlib/parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s), k0) = _menhir_stack in
                let _v : 'tv_command = let k =
                  let k = k0 in
                  
# 73 "smtlib/parser.mly"
                           ( Keyword k )
# 3618 "smtlib/parser.ml"
                  
                in
                
# 69 "smtlib/parser.mly"
                                  ( SetInfo (k,()) )
# 3624 "smtlib/parser.ml"
                 in
                _menhir_goto_command _menhir_env _menhir_stack _menhir_s _v) : 'freshtv208)) : 'freshtv210)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv211 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 3634 "smtlib/parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv212)) : 'freshtv214)) : 'freshtv216)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv217 * _menhir_state) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv218)) : 'freshtv220)) : 'freshtv222)
    | Tokens.SETLOGIC ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv237 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _tok = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv235 * _menhir_state) = _menhir_stack in
        let (_tok : Tokens.token) = _tok in
        ((match _tok with
        | Tokens.SYMBOL _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv231 * _menhir_state) = Obj.magic _menhir_stack in
            let (_v : (
# 12 "smtlib/parser.mly"
      (string)
# 3659 "smtlib/parser.ml"
            )) = _v in
            ((let _menhir_stack = (_menhir_stack, _v) in
            let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv229 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 3667 "smtlib/parser.ml"
            )) = _menhir_stack in
            let (_tok : Tokens.token) = _tok in
            ((match _tok with
            | Tokens.RPAREN ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv225 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 3676 "smtlib/parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let _ = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv223 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 3683 "smtlib/parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s), s0) = _menhir_stack in
                let _v : 'tv_command = let sym =
                  let s = s0 in
                  
# 72 "smtlib/parser.mly"
                         ( Symbol s )
# 3691 "smtlib/parser.ml"
                  
                in
                
# 40 "smtlib/parser.mly"
                                    (
    SetLogic sym
  )
# 3699 "smtlib/parser.ml"
                 in
                _menhir_goto_command _menhir_env _menhir_stack _menhir_s _v) : 'freshtv224)) : 'freshtv226)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv227 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 3709 "smtlib/parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv228)) : 'freshtv230)) : 'freshtv232)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv233 * _menhir_state) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv234)) : 'freshtv236)) : 'freshtv238)
    | Tokens.SETOPTION ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv253 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _tok = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv251 * _menhir_state) = _menhir_stack in
        let (_tok : Tokens.token) = _tok in
        ((match _tok with
        | Tokens.KEYWORD _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv247 * _menhir_state) = Obj.magic _menhir_stack in
            let (_v : (
# 12 "smtlib/parser.mly"
      (string)
# 3734 "smtlib/parser.ml"
            )) = _v in
            ((let _menhir_stack = (_menhir_stack, _v) in
            let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv245 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 3742 "smtlib/parser.ml"
            )) = _menhir_stack in
            let (_tok : Tokens.token) = _tok in
            ((match _tok with
            | Tokens.RPAREN ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv241 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 3751 "smtlib/parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let _ = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv239 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 3758 "smtlib/parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s), k0) = _menhir_stack in
                let _v : 'tv_command = let k =
                  let k = k0 in
                  
# 73 "smtlib/parser.mly"
                           ( Keyword k )
# 3766 "smtlib/parser.ml"
                  
                in
                
# 67 "smtlib/parser.mly"
                                    ( SetOption (k,()) )
# 3772 "smtlib/parser.ml"
                 in
                _menhir_goto_command _menhir_env _menhir_stack _menhir_s _v) : 'freshtv240)) : 'freshtv242)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv243 * _menhir_state) * (
# 12 "smtlib/parser.mly"
      (string)
# 3782 "smtlib/parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv244)) : 'freshtv246)) : 'freshtv248)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv249 * _menhir_state) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv250)) : 'freshtv252)) : 'freshtv254)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv255 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv256)) : 'freshtv258)

and program : (Lexing.lexbuf -> Tokens.token) -> Lexing.lexbuf -> (
# 28 "smtlib/parser.mly"
      (Syntax.command list)
# 3804 "smtlib/parser.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env =
      let (lexer : Lexing.lexbuf -> Tokens.token) = lexer in
      let (lexbuf : Lexing.lexbuf) = lexbuf in
      ((let _tok = lexer lexbuf in
      {
        _menhir_lexer = lexer;
        _menhir_lexbuf = lexbuf;
        _menhir_token = _tok;
        _menhir_startp = lexbuf.Lexing.lex_start_p;
        _menhir_endp = lexbuf.Lexing.lex_curr_p;
        _menhir_shifted = max_int;
        }) : _menhir_env)
    in
    Obj.magic (let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv3) = () in
    ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1) = _menhir_stack in
    let (_tok : Tokens.token) = _tok in
    ((match _tok with
    | Tokens.LPAREN ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0) : 'freshtv2)) : 'freshtv4))



