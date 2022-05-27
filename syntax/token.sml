
structure Token =
  struct
    type symbol = Symbol.symbol
    type span = Span.span

    datatype token =
      UIDENT of symbol Node.t
    | LIDENT of symbol Node.t
    | ULIDENT of symbol Node.t
    | SIDENT of symbol Node.t
    | USIDENT of symbol Node.t (* uppercase symbolic identifier *)
    | TYVAR of symbol Node.t

    | NUMBER of int Node.t
    | REAL of real Node.t
    | WORD of string Node.t
    | STRING of string Node.t
    | CHAR of char Node.t

    | LPAREN of span
    | RPAREN of span
    | LBRACKET of span
    | RBRACKET of span
    | LBRACE of span
    | RBRACE of span

    | ARROW of span
    | BAR of span
    | COLON of span
    | COMMA of span
    | DARROW of span
    | DOT of span
    | ELLIPSIS of span
    | EQUAL of span
    | SEAL of span
    | SEMICOLON of span
    | TIMES of span
    | UNDERSCORE of span
    | HASH of span

    | ABSTYPE of span
    | AND of span
    | ANDALSO of span
    | AS of span
    | CASE of span
    | DATATYPE of span
    | DO of span
    | ELSE of span
    | END of span
    | EQTYPE of span
    | EXCEPTION of span
    | FN of span
    | FUN of span
    | FUNCTOR of span
    | HANDLE of span
    | IF of span
    | IN of span
    | INCLUDE of span
    | LET of span
    | LOCAL of span
    | OF of span
    | OP of span
    | OPEN of span
    | ORELSE of span
    | RAISE of span
    | REC of span
    | REF of span
    | SHARING of span
    | SIG of span
    | SIGNATURE of span
    | STRUCT of span
    | STRUCTURE of span
    | THEN of span
    | TYPE of span
    | VAL of span
    | WHERE of span
    | WHILE of span
    | WITH of span
    | WITHTYPE of span

    | INFIX of span
    | INFIXR of span
    | NONFIX of span

    | EOF of span

    val node_eq = Node.location_insensitive_eq
    fun insensitive_compare token1 token2 =
      case (token1, token2) of
        (UIDENT node1, UIDENT node2) => node_eq Symbol.eq (node1, node2)
      | (LIDENT node1, LIDENT node2) => node_eq Symbol.eq (node1, node2)
      | (ULIDENT node1, ULIDENT node2) => node_eq Symbol.eq (node1, node2)
      | (SIDENT node1, SIDENT node2) => node_eq Symbol.eq (node1, node2)
      | (USIDENT node1, USIDENT node2) => node_eq Symbol.eq (node1, node2)
      | (TYVAR node1, TYVAR node2) => node_eq Symbol.eq (node1, node2)


      | (NUMBER node1, NUMBER node2) => node_eq (op=) (node1, node2)
      | (WORD node1, WORD node2) => node_eq (op=) (node1, node2)
      | (STRING node1, STRING node2) => node_eq (op=) (node1, node2)
      | (CHAR node1, CHAR node2) => node_eq (op=) (node1, node2)
      | (REAL node1, REAL node2) => true (* sure, why not *)

      | (LPAREN _, LPAREN _) => true
      | (RPAREN _, RPAREN _) => true
      | (LBRACKET _, LBRACKET _) => true
      | (RBRACKET _, RBRACKET _) => true
      | (LBRACE _, LBRACE _) => true
      | (RBRACE _, RBRACE _) => true

      | (ARROW _, ARROW _) => true
      | (BAR _, BAR _) => true
      | (COLON _, COLON _) => true
      | (COMMA _, COMMA _) => true
      | (DARROW _, DARROW _) => true
      | (DOT _, DOT _) => true
      | (ELLIPSIS _, ELLIPSIS _) => true
      | (EQUAL _, EQUAL _) => true
      | (SEAL _, SEAL _) => true
      | (SEMICOLON _, SEMICOLON _) => true
      | (TIMES _, TIMES _) => true
      | (UNDERSCORE _, UNDERSCORE _) => true

      | (ABSTYPE _, ABSTYPE _) => true
      | (AND _, AND _) => true
      | (ANDALSO _, ANDALSO _) => true
      | (AS _, AS _) => true
      | (CASE _, CASE _) => true
      | (DATATYPE _, DATATYPE _) => true
      | (DO _, DO _) => true
      | (ELSE _, ELSE _) => true
      | (END _, END _) => true
      | (EQTYPE _, EQTYPE _) => true
      | (EXCEPTION _, EXCEPTION _) => true
      | (FN _, FN _) => true
      | (FUN _, FUN _) => true
      | (FUNCTOR _, FUNCTOR _) => true
      | (HANDLE _, HANDLE _) => true
      | (IF _, IF _) => true
      | (IN _, IN _) => true
      | (INCLUDE _, INCLUDE _) => true
      | (LET _, LET _) => true
      | (LOCAL _, LOCAL _) => true
      | (OF _, OF _) => true
      | (OP _, OP _) => true
      | (OPEN _, OPEN _) => true
      | (ORELSE _, ORELSE _) => true
      | (RAISE _, RAISE _) => true
      | (REC _, REC _) => true
      | (REF _, REF _) => true
      | (SHARING _, SHARING _) => true
      | (SIG _, SIG _) => true
      | (SIGNATURE _, SIGNATURE _) => true
      | (STRUCT _, STRUCT _) => true
      | (STRUCTURE _, STRUCTURE _) => true
      | (THEN _, THEN _) => true
      | (TYPE _, TYPE _) => true
      | (VAL _, VAL _) => true
      | (WHERE _, WHERE _) => true
      | (WITH _, WITH _) => true
      | (WITHTYPE _, WITHTYPE  _) => true

      | (INFIX _, INFIX _) => true
      | (INFIXR _, INFIXR _) => true
      | (NONFIX _, NONFIX _) => true

      | (EOF _, EOF _) => true
      | _ => false

    fun token_to_string tok =
      case tok of
        UIDENT node => Symbol.toValue (Node.getVal node)
      | LIDENT node => Symbol.toValue (Node.getVal node)
      | ULIDENT node => Symbol.toValue (Node.getVal node)
      | SIDENT node => Symbol.toValue (Node.getVal node)
      | USIDENT node => Symbol.toValue (Node.getVal node)
      | TYVAR node => Symbol.toValue (Node.getVal node)

      | NUMBER node => Int.toString (Node.getVal node)
      | REAL node => Real.toString (Node.getVal node)
      | WORD node => Node.getVal node
      | STRING node => Node.getVal node
      | CHAR node => Char.toString (Node.getVal node)

      | LPAREN _ => "("
      | RPAREN _ => ")"
      | LBRACKET _ => "["
      | RBRACKET _ => "]"
      | LBRACE _ => "{"
      | RBRACE _ => "}"

      | ARROW _ => "->"
      | BAR _ => "|"
      | COLON _ => ":"
      | COMMA _ => ","
      | DARROW _ => "=>"
      | DOT _ => "."
      | ELLIPSIS _ => "..."
      | EQUAL _ => "="
      | SEAL _ => ":>"
      | SEMICOLON _ => ";"
      | TIMES _ => "*"
      | UNDERSCORE _ => "_"
      | HASH _ => "#"

      | ABSTYPE _ => "abstype"
      | AND _ => "and"
      | ANDALSO _ => "andalso"
      | AS _ => "as"
      | CASE _ => "case"
      | DATATYPE _ => "datatype"
      | DO _ => "do"
      | ELSE _ => "else"
      | END _ => "end"
      | EQTYPE _ => "eqtype"
      | EXCEPTION _ => "exception"
      | FN _ => "fn"
      | FUN _ => "fun"
      | FUNCTOR _ => "functor"
      | HANDLE _ => "handle"
      | IF _ => "if"
      | IN _ => "in"
      | INCLUDE _ => "include"
      | LET _ => "let"
      | LOCAL _ => "local"
      | OF _ => "of"
      | OP _ => "op"
      | OPEN _ => "open"
      | ORELSE _ => "orelse"
      | RAISE _ => "raise"
      | REC _ => "rec"
      | REF _ => "ref"
      | SHARING _ => "sharing"
      | SIG _ => "sig"
      | SIGNATURE _ => "signature"
      | STRUCT _ => "struct"
      | STRUCTURE _ => "structure"
      | THEN _ => "then"
      | TYPE _ => "type"
      | VAL _ => "val"
      | WHERE _ => "where"
      | WHILE _ => "while"
      | WITH _ => "with"
      | WITHTYPE _ => "withtype"

      | INFIX _ => "infix"
      | INFIXR _ => "infixr"
      | NONFIX _ => "nonfix"

      | EOF _ => "eof"

  end
