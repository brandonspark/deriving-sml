
signature LEXER =
  sig
    val lex : char Stream.stream -> Token.token Stream.stream
    val lex_string : string -> Token.token list
    val lex_file : string -> Token.token list
  end

structure Lexer :> LEXER =
  struct
    open Span
    open Token

    structure Table = SymbolHashTable

    fun revappend l1 l2 =
      (case l1 of
        x :: rest =>
          revappend rest (x :: l2)
      | [] => l2)

    fun mktok str tok =
      fn span => tok (Symbol.fromValue str, span)

    fun mktoknode str tok =
      fn span => tok (Node.create (Symbol.fromValue str, span))

    val sml_keywords_list =
      [
        ("::", mktoknode "::" USIDENT),
        ("true", mktoknode "true" ULIDENT),
        ("false", mktoknode "false" ULIDENT),
        ("nil", mktoknode "nil" ULIDENT),

        ("->", ARROW),
        ("|", BAR),
        (":", COLON),
        ("=>", DARROW),
        ("...", ELLIPSIS),
        (".", DOT),
        ("=", EQUAL),
        (":>", SEAL),
        ("*", TIMES),
        ("#", HASH),

        ("abystype", ABSTYPE),
        ("and", AND),
        ("andalso", ANDALSO),
        ("as", AS),
        ("case", CASE),
        ("datatype", DATATYPE),
        ("do", DO),
        ("else", ELSE),
        ("end", END),
        ("eqtype", EQTYPE),
        ("exception", EXCEPTION),
        ("fn", FN),
        ("fun", FUN),
        ("functor", FUNCTOR),
        ("handle", HANDLE),
        ("if", IF),
        ("in", IN),
        ("include", INCLUDE),
        ("let", LET),
        ("local", LOCAL),
        ("of", OF),
        ("op", OP),
        ("open", OPEN),
        ("orelse", ORELSE),
        ("raise", RAISE),
        ("rec", REC),
        ("ref", REF),
        ("sharing", SHARING),
        ("sig", SIG),
        ("signature", SIGNATURE),
        ("struct", STRUCT),
        ("structure", STRUCTURE),
        ("then", THEN),
        ("type", TYPE),
        ("val", VAL),
        ("where", WHERE),
        ("while", WHILE),
        ("with", WITH),
        ("withtype", WITHTYPE),

        ("infix", INFIX),
        ("infixr", INFIXR),
        ("nonfix", NONFIX)
      ]

    val sml_keywords : (span -> token) Table.table = Table.table 60

    val () =
      List.app
      (fn (str, token) => Table.insert sml_keywords (Symbol.fromValue str) token)
      sml_keywords_list

    (* check for consecutive underscores *)
    fun isLegal str =
      let
        fun loop l =
          (case l of
             nil => true
           | ch :: rest =>
               if ch = #"_" then
                 loop' rest
               else
                 loop rest)

        and loop' l =
          (case l of
             nil => true
           | ch :: rest =>
               if ch = #"_" then
                 false
               else
                 loop rest)
      in
        loop (String.explode str)
      end

    fun identify table str span =
      let
        val sym = Symbol.fromValue str
      in
       (case Table.find table sym of
          NONE =>
            if isLegal str then
              let
                val ch = String.sub (str, 0)
              in
                if Char.isLower ch then
                  LIDENT (Node.create (sym, span))
                else if Char.isUpper ch then
                  UIDENT (Node.create (sym, span))
                else
                  SIDENT (Node.create (sym, span))
              end
            else
              raise Error.SyntaxError ("illegal identifier", #1 span)

        | SOME tokfn =>
             tokfn span)
      end

    fun longIdentify table acc curr currstart pos l =
     (case l of
         [] =>
            (* The lexer ensures that the longid is nonempty, and longIdentify
               never calls itself recursively with curr=nil, so curr is not nil here.
            *)
            rev (identify table (implode (rev curr)) (currstart, pos)
                 :: acc)

       | #"." :: rest =>
          let
             (* The lexer ensures that the longid cannot begin with dot, and the
                symbol case below deals with any case when there are consecutive dots,
                so curr can never be nil at this point.
             *)
             val acc =
                DOT (pos, pos+1)
                :: identify table (implode (rev curr)) (currstart, pos)
                :: acc
          in
             (case rest of
                 nil =>
                    (* The lexer ensures that the final identifier in a longid is nonempty,
                       so rest cannot be nil here.
                    *)
                    raise (Fail "impossible")

               | ch :: rest' =>
                    if Char.isAlpha ch then
                       longIdentify table acc [ch] (pos+1) (pos+2) rest'
                    else
                       (* The lexer ensures that the remaining characters are all symbols. *)
                       let
                          val str = implode rest
                       in
                          rev (identify table str (pos+1, pos+1 + size str)
                               :: acc)
                       end)
          end

       | ch :: rest =>
          longIdentify table acc (ch :: curr) currstart (pos+1) rest)


    open Stream
    open Error

    structure Arg =
      struct
        structure Streamable = StreamStreamable

        type symbol = char
        val ord = Char.ord

        datatype tlex = LEX of char stream -> t
        withtype t = tlex -> pos -> token front

        type u = pos -> char stream * pos
        type v = pos -> char list -> char list * char stream * pos

        type self = { comment : symbol Streamable.t -> u,
                      main : symbol Streamable.t -> t,
                      string : symbol Streamable.t -> v }
        type info = { match : symbol list,
                      len : int,
                      start : symbol Streamable.t,
                      follow : symbol Streamable.t,
                      self : self }

        fun action f ({ match, len, follow, self, ...}: info) (k as LEX cont) pos =
          Cons (f (match, len, pos), lazy (fn () => cont follow k (pos + len)))

        fun simple tokfn ({ match, len, follow, self, ...}: info) (k as LEX cont) pos =
          Cons (tokfn (pos, pos + len), lazy (fn () => cont follow k (pos + len)))

        fun skip ({ len, follow, self, ...} : info) (k as LEX cont) pos =
          cont follow k (pos + len)

        val main_ident =
          action
          (fn (match, len, pos) => identify sml_keywords (implode match) (pos, pos +
          len))

        fun main_longid ({match, len, follow, self, ...}:info) (k as LEX cont) pos =
          front
          (Stream.@ (Stream.fromList (longIdentify sml_keywords [] [] pos pos match),
                     lazy (fn () => cont follow k (pos + len))))


        val tyvar =
          action
          (fn (match, len, pos) =>
            TYVAR (Node.create (Symbol.fromValue (implode match), (pos, pos + len))))

        fun mk_number f =
          action
          (fn (match, len, pos) =>
            (case f (implode match) of
              SOME n => NUMBER (Node.create (n, (pos, pos + len)))
            | NONE =>
                raise SyntaxError ("illegal number", pos)))

        val number = mk_number Int.fromString
        val nnumber = mk_number Int.fromString
        val hex_number =
          mk_number (StringCvt.scanString (Int.scan StringCvt.HEX))
        val hex_nnumber =
          mk_number (StringCvt.scanString (Int.scan StringCvt.HEX))

        val wordlit =
          action
          (fn (match, len, pos) =>
            (case List.drop (match, 2) of
              #"x"::rest => WORD (Node.create (implode rest, (pos, pos + len)))
            | other => WORD (Node.create (implode other, (pos, pos + len)))))

        val reallit =
          action
          (fn (match, len, pos) =>
            (case Real.fromString (implode match) of
              SOME n => REAL (Node.create (n, (pos, pos + len)))
            | NONE => raise SyntaxError ("illegal real", pos)))

        fun enter_comment ({len, follow, self, ...}: info) (k as LEX cont) pos =
          let
            val (follow', pos') = #comment self follow (pos + len)
          in
            cont follow' k pos'
          end

        fun enter_string ({len, follow, self, ...}: info) (k as LEX cont) pos =
          let
            (* Suppose you have something like this:
             * "  \"   a \"  "
             * This will get parsed by #string into a list which looks like:
             * [ #"\"" #"a", #"\"" ]
             *
             * The backslashes don't get parsed!
             *)
            val (chars, follow', pos') = #string self follow (pos + len) []

            val correct =
              String.implode
                ( List.concatMap
                    (fn #"\"" => [#"\\", #"\""]
                    | #"\n" => [#"\\", #"n"]
                    (* TODO: fix other special characters? *)
                    | other => [other]
                    )
                    (List.rev chars)
                )
          in
            Cons (STRING (Node.create (correct, (pos, pos'))),
                  lazy (fn () => cont follow' k pos'))
          end

        fun enter_char ({len, follow, self, ...}: info) (k as LEX cont) pos =
          let
            val (chars, follow', pos') = #string self follow (pos + len) []
          in
            case chars of
              [ch] =>
                Cons (CHAR (Node.create (ch, (pos, pos'))),
                      lazy (fn () => cont follow' k pos'))
            | _ =>
                raise SyntaxError ("illegal character constant", pos)
          end

        val lparen = simple LPAREN
        val rparen = simple RPAREN
        val lbracket = simple LBRACKET
        val rbracket = simple RBRACKET
        val lbrace = simple LBRACE
        val rbrace = simple RBRACE
        val comma = simple COMMA
        val underscore = simple UNDERSCORE

        val semicolon =
          action
          (fn (_, len, pos) =>  SEMICOLON (pos, pos+len))

        fun eof _ _ pos =
          Cons (EOF (pos, pos), eager Nil)

        fun error ({follow, ...}: info) _ pos =
          (print ((String.implode (Stream.toList follow)) ^ "\n");
           raise SyntaxError ("illegal lexeme", pos))

        (* comment *)

        fun reenter_comment ({ len, follow, self, ...}: info) pos =
          let
            val (follow', pos') = #comment self follow (pos + len)
          in
            #comment self follow' pos'
          end

        fun exit_comment ({ len, follow, ...}: info) pos =
          (follow, pos + len)

        fun comment_skip ({ len, follow, self, ...} : info) pos =
          #comment self follow (pos + len)

        fun unclosed_comment _ pos = raise SyntaxError ("unclosed comment", pos)

        fun comment_error _ pos = raise SyntaxError ("illegal character", pos)


        (* string *)

        fun string_action f ({ match, len, follow, self, ...}: info) pos acc =
          #string self follow (pos + len) (f (match, acc))

        val string_elem =
          string_action
          (fn (match, acc) => revappend match acc)

        val string_newline =
          string_action
          (fn (_, acc) => #"\n" :: acc)

        val string_backslash =
          string_action
          (fn (_, acc) => #"\\" :: acc)

        val string_quote =
          string_action
          (fn (_, acc) => #"\"" :: acc)

        fun hexdigit ch =
          let val i = Char.ord ch
          in
            if i <= Char.ord #"9" then
               i - Char.ord #"0"
            else if i <= Char.ord #"F" then
               i - Char.ord #"A" + 10
            else
               i - Char.ord #"f"
          end

        val string_hex2 =
          string_action
          (fn ([_, _, a, b], acc) => Char.chr (hexdigit a * 16 + hexdigit b) :: acc
          | _ => raise (Fail "impossible by lexer design"))

        fun string_skip ({ len, follow, self, ... }:info) pos acc =
          #string self follow (pos+len) acc

        fun exit_string ({ len, follow, ... }:info) pos acc =
          (acc, follow, pos+len)

        fun unclosed_string _ pos _ = raise (SyntaxError ("unclosed string", pos))

        fun string_error _ pos _ = raise (SyntaxError ("illegal character", pos))

      end

    structure Input =
      struct

       structure Streamable = StreamStreamable
       structure Arg = Arg
      end

    structure LexMain =
      LexMainFun (Input)

    fun doLex f s = lazy (fn () => f s (Arg.LEX f) 0)

    fun lex s = doLex LexMain.main s

    fun lex_string s = Stream.toList (lex (Stream.fromList (String.explode s)))

    fun lex_file s = lex_string (TextIO.inputAll (TextIO.openIn s))
  end
