type regexp =
 | V  
 | E
 | C of char
 | U of regexp * regexp 
 | P of regexp * regexp 
 | S of regexp

module Parser_regexp = struct

  
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | RPAREN
    | LPAREN
    | EPS
    | EOF
    | EMP
    | CONC
    | CHAR of (
       (char)
  )
    | AST
    | ALT
  
end

include MenhirBasics

let _eRR =
  MenhirBasics.Error

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState12
  | MenhirState7
  | MenhirState6
  | MenhirState1
  | MenhirState0


let rec _menhir_goto_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState6 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv47 * _menhir_state * 'tv_term)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv45 * _menhir_state * 'tv_term)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (e1 : 'tv_term)), _, (e2 : 'tv_expr)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_expr = 
                                ( P (e1, e2) )
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv46)) : 'freshtv48)
    | MenhirState12 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv51 * _menhir_state * 'tv_term)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv49 * _menhir_state * 'tv_term)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (e1 : 'tv_term)), _, (e2 : 'tv_expr)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_expr = 
                                ( U (e1, e2) )
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv50)) : 'freshtv52)
    | MenhirState1 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv59 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv55 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv53 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (e : 'tv_expr)) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_atom = 
                                ( e )
             in
            _menhir_goto_atom _menhir_env _menhir_stack _menhir_s _v) : 'freshtv54)) : 'freshtv56)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv57 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv58)) : 'freshtv60)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv73 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EOF ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv69 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv67 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (le : 'tv_expr)) = _menhir_stack in
            let _2 = () in
            let _v : (
       (regexp)
            ) = 
                                ( le )
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv65) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
       (regexp)
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv63) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
       (regexp)
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv61) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let ((_1 : (
       (regexp)
            )) : (
       (regexp)
            )) = _v in
            (Obj.magic _1 : 'freshtv62)) : 'freshtv64)) : 'freshtv66)) : 'freshtv68)) : 'freshtv70)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv71 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv72)) : 'freshtv74)
    | _ ->
        let (() : unit) = () in
        ((Printf.fprintf stderr "Internal failure -- please contact the parser generator's developers.\n%!";
        assert false) : 'freshtv75)

and _menhir_goto_term : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_term -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState0 | MenhirState12 | MenhirState6 | MenhirState1 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv39 * _menhir_state * 'tv_term) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ALT ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv31 * _menhir_state * 'tv_term) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | CHAR _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
            | EMP ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState12
            | EPS ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState12
            | LPAREN ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState12
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState12) : 'freshtv32)
        | CONC ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv33 * _menhir_state * 'tv_term) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | CHAR _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
            | EMP ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState6
            | EPS ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState6
            | LPAREN ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState6
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState6) : 'freshtv34)
        | EOF | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv35 * _menhir_state * 'tv_term) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (e : 'tv_term)) = _menhir_stack in
            let _v : 'tv_expr = 
                                ( e )
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv36)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv37 * _menhir_state * 'tv_term) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv38)) : 'freshtv40)
    | MenhirState7 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv43 * _menhir_state * 'tv_factor) * _menhir_state * 'tv_term) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv41 * _menhir_state * 'tv_factor) * _menhir_state * 'tv_term) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (e1 : 'tv_factor)), _, (e2 : 'tv_term)) = _menhir_stack in
        let _v : 'tv_term = 
                                ( P (e1, e2) )
         in
        _menhir_goto_term _menhir_env _menhir_stack _menhir_s _v) : 'freshtv42)) : 'freshtv44)

and _menhir_goto_factor : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_factor -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv29 * _menhir_state * 'tv_factor) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CHAR _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
    | EMP ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | EPS ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | LPAREN ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | ALT | CONC | EOF | RPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv27 * _menhir_state * 'tv_factor) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (e : 'tv_factor)) = _menhir_stack in
        let _v : 'tv_term = 
                                ( e )
         in
        _menhir_goto_term _menhir_env _menhir_stack _menhir_s _v) : 'freshtv28)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState7) : 'freshtv30)

and _menhir_goto_atom : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_atom -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv25 * _menhir_state * 'tv_atom) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AST ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv21 * _menhir_state * 'tv_atom) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv19 * _menhir_state * 'tv_atom) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (e : 'tv_atom)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_factor = 
                                ( S e )
         in
        _menhir_goto_factor _menhir_env _menhir_stack _menhir_s _v) : 'freshtv20)) : 'freshtv22)
    | ALT | CHAR _ | CONC | EMP | EOF | EPS | LPAREN | RPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv23 * _menhir_state * 'tv_atom) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (e : 'tv_atom)) = _menhir_stack in
        let _v : 'tv_factor = 
                                ( e )
         in
        _menhir_goto_factor _menhir_env _menhir_stack _menhir_s _v) : 'freshtv24)) : 'freshtv26)

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState12 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv9 * _menhir_state * 'tv_term)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv10)
    | MenhirState7 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv11 * _menhir_state * 'tv_factor) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv12)
    | MenhirState6 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv13 * _menhir_state * 'tv_term)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv14)
    | MenhirState1 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv15 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv16)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv17) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv18)

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CHAR _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _v
    | EMP ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | EPS ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | LPAREN ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState1

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv7) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_atom = 
                                ( E )
     in
    _menhir_goto_atom _menhir_env _menhir_stack _menhir_s _v) : 'freshtv8)

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv5) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_atom = 
                                ( V )
     in
    _menhir_goto_atom _menhir_env _menhir_stack _menhir_s _v) : 'freshtv6)

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
       (char)
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv3) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((c : (
       (char)
    )) : (
       (char)
    )) = _v in
    ((let _v : 'tv_atom = 
                                ( C c )
     in
    _menhir_goto_atom _menhir_env _menhir_stack _menhir_s _v) : 'freshtv4)

and _menhir_discard : _menhir_env -> _menhir_env =
  fun _menhir_env ->
    let lexer = _menhir_env._menhir_lexer in
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and regexpr : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
       (regexp)
) =
  fun lexer lexbuf ->
    let _menhir_env =
      let (lexer : Lexing.lexbuf -> token) = lexer in
      let (lexbuf : Lexing.lexbuf) = lexbuf in
      ((let _tok = Obj.magic () in
      {
        _menhir_lexer = lexer;
        _menhir_lexbuf = lexbuf;
        _menhir_token = _tok;
        _menhir_error = false;
      }) : _menhir_env)
    in
    Obj.magic (let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1) = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    ((let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CHAR _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | EMP ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | EPS ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LPAREN ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0) : 'freshtv2))

  



end

module Lexer_regexp = struct
 
  open Parser_regexp

  exception Error of string


  let __ocaml_lex_tables = {
    Lexing.lex_base =
    "\000\000\245\255\246\255\247\255\248\255\249\255\250\255\251\255\
      \252\255\253\255\254\255\255\255";
    Lexing.lex_backtrk =
    "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
      \255\255\255\255\255\255\255\255";
    Lexing.lex_default =
    "\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\000\000";
    Lexing.lex_trans =
    "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\011\000\011\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \011\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \004\000\003\000\007\000\009\000\000\000\000\000\008\000\000\000\
      \006\000\005\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
      \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
      \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
      \010\000\010\000\010\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
      \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
      \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
      \010\000\010\000\010\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \002\000";
    Lexing.lex_check =
    "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
      \255\255\000\000\000\000\255\255\255\255\255\255\255\255\255\255\
      \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
      \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
      \000\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
      \000\000\000\000\000\000\000\000\255\255\255\255\000\000\255\255\
      \000\000\000\000\255\255\255\255\255\255\255\255\255\255\255\255\
      \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
      \255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\255\255\255\255\255\255\255\255\255\255\
      \255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\255\255\255\255\255\255\255\255\255\255\
      \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
      \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
      \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
      \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
      \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
      \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
      \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
      \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
      \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
      \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
      \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
      \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
      \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
      \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
      \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
      \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
      \000\000";
    Lexing.lex_base_code =
    "";
    Lexing.lex_backtrk_code =
    "";
    Lexing.lex_default_code =
    "";
    Lexing.lex_trans_code =
    "";
    Lexing.lex_check_code =
    "";
    Lexing.lex_code =
    "";
  }

  let rec tokenize lexbuf =
    __ocaml_lex_tokenize_rec lexbuf 0
  and __ocaml_lex_tokenize_rec lexbuf __ocaml_lex_state =
    match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
        | 0 ->
                                        ( tokenize lexbuf )

    | 1 ->
  let
                          s
  = Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
                                        ( CHAR s )

    | 2 ->
                                        ( ALT )

    | 3 ->
                                        ( CONC )

    | 4 ->
                                        ( AST )

    | 5 ->
                                        ( EMP )

    | 6 ->
                                        ( EPS )

    | 7 ->
                                        ( LPAREN )

    | 8 ->
                                        ( RPAREN )

    | 9 ->
                                        ( EOF )

    | 10 ->
        ( raise (Error (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) )

    | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
        __ocaml_lex_tokenize_rec lexbuf __ocaml_lex_state

  ;;
end

(* --------------------------------- fim lexing/parsing code ----------------------------------------------------- *)

open Parser_regexp


(* funcao principal de leitura de uma expressao regular (a partir de uma string) *)
let regexp st =
  let linebuf = Lexing.from_string st in
  try regexpr Lexer_regexp.tokenize linebuf
  with _ -> failwith "regexp: input problem"


(* **************************************************************************************************************** *)
(* ********************************************   Comecar aqui **************************************************** *)

(* 
  Referências: 
   - http://www.di.ubi.pt/~desousa/TC/regexp.ml
   - [Aulas 04-02 a 04-03] https://online.stanford.edu/courses/soe-ycscs1-compilers
   - http://www.di.ubi.pt/~desousa/TC/aula_tc3-pp.pdf
   - [Secção Rules] https://en.wikipedia.org/wiki/Thompson%27s_construction

  18/06/2020

  Autores: 
  37283 - Pedro Moreira
  39973 - Dário Santos
*)

(* 
  É utilizada uma excepção para simular a instrução break,
  esta excepção é utilizada durante o algoritmo de aceitação
  para terminarmos a execução do programa.
*)
exception Break of bool

(*
  A função ```nfa_of_regexp``` converte uma expressão regular para um NFA
  utilizando para isso um conjunto de funções.
  O algoritmo utilizado na nossa implementação foi o de Thompson e está 
  explicado na função ```convert```.
*)
let nfa_of_regexp expr =
  (*
    A tabela de hash ```transicoes``` guarda as transições de um estado para outro estado através de um caracter.

    No algoritmo de construção de um NFA que utilizamos, construção de Thompson, um estado pode no máximo ter uma transição
    que consome um caractere.

    A chave utilizada nesta tabela é (int, char) e o seu valor é um int. ((int, char), int)
    
    Que de forma mais direta representa:

    ((estado, consumo), estado_seguinte)
  *)
  let transicoes = Hashtbl.create 16 in
  (*
    A tabela de hash ```transicoes_eps``` guarda as transições epsilon de um estado.

    No algoritmo de construção de um NFA que utilizamos, construção de Thompson, um estado pode ter uma ou duas transições epsilon.

    Logo utilizámos o estado como chave e uma lista de estados com valor (int, int list ref).

    A lista de estados representa os estados a um estado pode chegar através de uma transição epsilon.
  *)
  let transicoes_eps = Hashtbl.create 16 in
  (* 
    A variável ```estado``` é utilizada durante a construção do NFA e depois de terminada a construção do NFA
    representa o valor do estado final do NFA.
  *)
  let estado = ref 1 in
  (*
    A função ```add_epsilon_transition``` é uma função utilitária utilizada durante a construção do NFA
    e adiciona uma transição epsilon de i para f:
      i -(E)-> f
  
    Se já existirem transições epsilon para este estado, ou seja, ele já existe na tabela,
    então só temos que atualizar a lista dos estados de chegada.

    Se não existir, vai lançar a excepção Not_found e podemos utilizar isto para adicionar um novo elemento.

    Assim não existe shadowing na tabela.
  *)
  let add_epsilon_trasition i f =
    try
      let l = Hashtbl.find transicoes_eps i in
      l := f::!l
    with Not_found -> Hashtbl.add transicoes_eps i (ref [f])
  in
  (*
    A função ```convert``` converte uma expressão regular para um NFA *com expressões epsilon*,
    utilizando o algoritmo de Thompson.

    O algoritmo de Thompson tem 5 regras de construção:

      Epsilon:

        (1)--E-->((2))
      
      Caractere(c): 
      
        (1)--c-->((2))

      União(f, g):

             +--E-->(f)--E--+
        (1)--+              +-->((2))
             +--E-->(g)--E--+

      Produto(f, g):

        (1)--E-->(f)--E-->(g)--E-->((2))

      Star(s):
                    +--E--+
                    |     v
        (1) --E-->[(2) s (3)]--E--> ((4))
         |                            ^
         +-------------E--------------+


    As transições necessárias em cada estado são detalhadas na sua secção da função ```convert```.
  *)
  let rec convert = function
    | V       -> () (* Ignoramos o vazio *)
    | E       ->
      (* 
        O caso epsilon tem uma transição:
        1. inicio --E--> estado
      *)
      let inicio = !estado in
      estado := !estado + 1;

      (* 1. inicio --E--> estado *)
      add_epsilon_trasition inicio !estado
    | C c     ->
      (* 
        O caso caractere tem uma transição:
        1. inicio --c--> estado
      *)
      let inicio = !estado in
      estado := !estado + 1;
      (*1. inicio --c--> estado *)
      Hashtbl.add transicoes (inicio, c) !estado 
    | U (f, g)->
      (* 
        O caso União tem no mínimo 4 transições:
        1. inicio --E--> inicio de f
        2. inicio --E--> inicio de g
        3. fim de f --E--> fim
        4. fim de g --E--> fim
      *)
      let inicio = !estado in
      estado := !estado + 1;
      
      (* 1. inicio --E--> inicio de f *)
      add_epsilon_trasition inicio !estado;

      (* Converter a subexpressão regular f *)
      convert f;
      
      (* Guarda o valor do último estado de f *)
      let last_state_f = !estado in

      (* Estado inicial de G *)
      estado := !estado + 1;
      
      (* 2. inicio --E--> inicio de g *)
      add_epsilon_trasition inicio !estado;

      (* Converter a subexpressão regular g *)
      convert g;

      (* Guarda o valor do último estado de g *)
      let last_state_g = !estado in

      (* Estado final *)
      estado := !estado + 1;
      
      (* 3. fim de f --E--> fim *)
      add_epsilon_trasition last_state_f !estado;

      (* 4. fim de g --E--> fim *)
      add_epsilon_trasition last_state_g !estado
    | P (f,g) ->
      (* 
        O caso Produto tem no mínimo 4 transições:
        1. inicio --E--> inicio de f
        2. fim de f --E ou c--> inicio de g
        3. fim de g --E--> fim
      *)
      let inicio = !estado in
      estado := !estado + 1;
      
      (* 1. inicio --E--> inicio de f *)
      add_epsilon_trasition inicio !estado;

      (* Converter a subexpressão regular f *)
      convert f;
      
      (*
        Devido à forma como o algoritmo de conversão está implementado quando g for convertido,
        este vai buscar o valor da variável ```estado``` que vai ser o valor do último estado 
        da subexpressão f e adicionar uma transição de f para o início de g.
        Esta transição pode tanto ser com ou sem consumo (ser ou não epsilon).
      *)

      (* Converter a subexpressão regular g *)
      convert g;

      (* Guarda o valor do último estado de g *)
      let last_state_g = !estado in

      (* Estado final *)
      estado := !estado + 1;
      
      (* 3. fim de g --E--> fim *)
      add_epsilon_trasition last_state_g !estado
    | S s     ->
      (*
        O caso Star tem no mínimo 4 transições:
        1. inicio --E--> inicio de s
        2. fim de s --E--> fim
        3. fim de s --E--> inicio de s
        4. inicio --E--> fim
      *)
      let inicio = !estado in
      estado := !estado + 1;
      (* Guarda o primeiro estado de s *)
      let first_state_s = !estado in

      (* 1. inicio --E--> inicio de s *)
      add_epsilon_trasition inicio !estado;

      (* Converte a subsexpressão s *)
      convert s;

      (* Guarda o último estado de s *)
      let last_state_s = !estado in

      (* Estado final *)
      estado := !estado + 1;
      
      (* 2. fim de s --E--> fim *)
      add_epsilon_trasition last_state_s !estado;

      (* 3. fim de s --E--> inicio de s *)
      add_epsilon_trasition last_state_s first_state_s;

      (* 4. inicio --E--> fim *)
      add_epsilon_trasition inicio !estado
  in
  (*
    A função ```calcular_estados_epsilon``` é responsável por calcular os conjuntos de estados epsilon a que um
    estado com transições epsilon consegue atingir através de qualquer número de transições epsilon.
    Esta é uma otimização para não necessitarmos de estar sempre a recalcular estes conjuntos.

    Como é possível existirem ciclos entre estados, foi  utilizada uma lista que guarda 
    os estados já visitados para não serem recalculados.

    Exemplo:
      NFA:
       (1) --E--> (2) --E--> (3) --A--> (4) --E--> ((5))

      Conjuntos:  
       1: {2, 3} 
       2: {3}
       3: {}
       4: {5}
       5: {}
  *)
  let calcular_estados_epsilon transicoes_eps =
    let visitados = ref [] in
    let estados_seguintes_epsilon = Hashtbl.create 16 in

    (* A função ```marcar``` calcula o conjunto de estados seguintes de um estado *)
    let rec marcar estado = 
      if not (List.mem estado !visitados) then
      (
        visitados := estado::!visitados;

        (* 1. Verificar se existe estados epsilon deste estado *)
        let s = try !(Hashtbl.find transicoes_eps (estado)) with Not_found -> [] in
          
        let tmp = ref s in
        (* 2. Calcular seguintes *)
        List.iter (fun e ->
          (* calcula as dependencias *)
          marcar e;
          (* vamos buscar os seguintes dele *)
          let v = try !(Hashtbl.find estados_seguintes_epsilon e) with Not_found -> [] in
          tmp := !tmp@v
        ) s;

        (* 3. Adicionar *)
        Hashtbl.add estados_seguintes_epsilon estado tmp
      )
    in
    Hashtbl.iter (fun k _ -> marcar k) transicoes_eps;
    estados_seguintes_epsilon
  in
  (* Converte a expressão regular para NFA *)
  convert expr;
  (* Calcula os conjuntos de estados a que cada estado com transições epsilon consegue chegar, apenas com transições epsilon *)
  let estados_seguintes_epsilon = calcular_estados_epsilon transicoes_eps in
  (* Transições com consumo, conjuntos de estados atingíveis por epsilon, estado final*)
  transicoes, estados_seguintes_epsilon, !estado


(* 
  A função ```nfa_accept``` recebe um NFA e uma string e verifica se pelo menos uma parte da string é aceite pelo NFA.
*)
let nfa_accept nfa dna =
  (* Extraímos a informação do NFA *)
  let transicoes, estados_seguintes_epsilon, final = nfa in
  (* 
    A função ```normalize``` foi fornecida pelo Professor Doutor Simão Sousa nos recursos do problema D.

    normalize l = l sem duplicados, de forma eficiente ie. linear! *) 
  let normalize l =
    let tbl = Hashtbl.create (List.length l) in
    let f l e = 
      try 
        Hashtbl.find tbl e; l
        with Not_found -> Hashtbl.add tbl e (); e::l
      in
    List.rev (List.fold_left f [] l)
  in
  (* 
    A função ```next_states``` devolve uma lista com os estados a que podemos chegar partindo de um 
    dado estado e consumindo o caractere fornecido.
  *)
  let next_states state c = 
    try [Hashtbl.find transicoes (state, c)] with Not_found -> []
  in
  (* 
    A função ```accept``` é o coração do problema C, é responsável por verificar
    se alguma secção da string fornecida é aceite por um determinado automato.

    Uma palavra é aceite por um automato se ao chegarmos ao fim dessa palavra estivermos num estado final do automato.


    Pseudo código do algortimo de aceitação de um NFA, os estados atuais são inicializados a 1 pois este é o estado inicial:

      estados_atuais = [1]
      estados_epsilon = []
      estados_seguintes = []

      Para cada caractere na palavra w:
        Calcular os estados seguintes através de transições epsilon

        Calcula os estados seguintes através do consumo do caractere atual da palavra w

        Se não existem estados seguintes, termina.
        
        estados_atuais = estados_seguintes
        estados_epsilon = []
        estados_seguintes = []
      Fim

      Se o estado final pode ser atingido através dos esatdos atuais então
        É aceite
      Se não
        Não é aceite
    
    Como apenas necessitamos que parte da palavra seja aceite utilizamos fazemos duas alterações ao algoritmo de aceitação
    de um NFA:

      1. Forçamos o estado 1, o estado inicial, a estar sempre presente no conjunto de estados atuais. Assim adicionamos uma nova execução partindo
         do caractere atual da palavra às já presentes;
      2. Verificamos em todas as iterações se atingimos o estado final. Se sim, terminamos a execução utilizando a exceção Break.
  *)
  let accept dna =
    (* estado 1 é o inicial *)
    let estados_atuais = ref [1] in
    let estados_seguintes = ref [] in
    let estados_epsilon = ref [] in
    (* Se é ou não necessário pesquisar pelo estado final *)
    let research =
      (try 
        (* Consumir toda a cadeia de dna *)
        for i=0 to (String.length dna)-1 do
          (* Força a presença do estado 1 nos atuais *)
          if not (List.mem 1 !estados_atuais) then estados_atuais := 1::!estados_atuais;

          (* Ir buscar a que estados podemos chegar através de epsilon *)
          List.iter (fun s ->
            let v = try !(Hashtbl.find estados_seguintes_epsilon s) with Not_found -> [] in
            estados_epsilon := !estados_epsilon@v
          ) !estados_atuais;

          estados_epsilon := normalize !estados_epsilon;

          (* Se estamos no estado final termina *)
          if List.mem final !estados_atuais then raise (Break false);

          (* Se estamos no estado final termina *)
          if List.mem final !estados_epsilon then raise (Break false);

          (* Calcular os estados seguintes através de dna.(i) *)
          List.iter (fun s -> estados_seguintes := !estados_seguintes@(next_states s dna.[i])) !estados_atuais;

          (* Calcular os estados seguintes através de dna.(i) *)
          List.iter (fun s -> estados_seguintes := !estados_seguintes@(next_states s dna.[i])) !estados_epsilon;

          estados_seguintes := normalize !estados_seguintes;

          (* Guardar os seguintes como atuais e limpar os seguintes *)
          estados_atuais := !estados_seguintes;
          estados_seguintes := [];
          estados_epsilon := [];
        done;
        true
      with Break v -> v) in
    (* é preciso pesquisar por estados finais? *)
    if not research then true
    else
    (
      (* Ir buscar a que estados podemos chegar através de epsilon *)
      List.iter (fun s ->
        let v = try !(Hashtbl.find estados_seguintes_epsilon s) with Not_found -> [] in
        estados_epsilon := !estados_epsilon@v
      ) !estados_atuais;

      estados_atuais := !estados_atuais@(!estados_epsilon);

      (* Se chegarmos ao fim de dna e se em estados_atuais houver um estado final -> YES *)
      List.mem final !estados_atuais
    )
  in
  accept dna
    
(*
  Formato de entrada: 
   exp - expressão regular
   dna - cadeia de nucleotidos
*)
(* Expressão regular, que é convertida para regexp através da função regexp *)
let exp = regexp (read_line())
(* Cadeia de nucleotidos *)
let dna = read_line()
(* 1. Converte a expressão regular para nfa *)
(* NFA : transicoes, estados_seguintes_epsilon, estado final *)
let nfa = nfa_of_regexp exp

(* 2. Verifica se alguma secção da palavra é aceite pelo NFA, e realiza o output de acordo. *)
let _ = Printf.printf "%s\n" (if nfa_accept nfa dna then "YES" else "NO")
