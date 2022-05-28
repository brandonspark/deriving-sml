
signature LEXER =
  sig
    val lex : char Stream.stream -> CM_Token.token Stream.stream
    val lex_string : string -> CM_Token.token list
    val lex_file : string -> CM_Token.token list
  end

structure CM_Lexer :> LEXER =
  struct
    open Span
    open CM_Token

    fun revappend l1 l2 =
      (case l1 of
        x :: rest =>
          revappend rest (x :: l2)
      | [] => l2)

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
                      group_or_library : symbol Streamable.t -> t,
                      ifmode : symbol Streamable.t -> u,
                      is : symbol Streamable.t -> t,
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

        fun skip_main ({ len, follow, self, ...} : info) (k as LEX cont) pos =
          #main self follow k (pos + len)

        fun analyze_gl ({ match, len, follow, self, ... } : info) (k as LEX cont) pos =
          case String.implode match of
            "is" => (#is self follow (LEX (#is self)) (pos + len)(*; raise Fail (String.implode
            (Stream.toList follow))*))
          | _ => #group_or_library self follow (LEX (#group_or_library self)) (pos + len)

        fun skip_gl ({ len, follow, self, ...} : info) (k as LEX cont) pos =
          #group_or_library self follow (LEX (#group_or_library self)) (pos + len)

        fun enter_if ({ len, follow, self, ...} : info) (k as LEX cont) pos =
          let
            val (follow, pos) = #ifmode self follow (pos + len)
          in
            cont follow k pos
          end



        fun skip_if ({ len, follow, self, ...} : info) pos =
          #ifmode self follow (pos + len)

        fun exit_if ({ len, follow, self, ...} : info) pos =
          (follow, pos)

        fun skip_is ({ len, follow, self, ...} : info) (k as LEX cont) pos =
          #is self follow (LEX (#is self)) (pos + len)

        fun enter_group ({ len, follow, self, ...} : info) (k as LEX cont) pos =
          Cons ( GROUP (pos, pos + len)
               , lazy (fn () => #group_or_library self follow (LEX
               (#group_or_library self)) (pos + len))
               )

        fun enter_library ({ len, follow, self, ...} : info) (k as LEX cont) pos =
          Cons ( LIBRARY (pos, pos + len)
               , lazy (fn () => #group_or_library self follow (LEX
               (#group_or_library self)) (pos + len))
               )

        fun enter_is ({ len, follow, self, ...} : info) (k as LEX cont) pos =
          Cons ( IS (pos, pos + len)
               , lazy (fn () => #is self follow (LEX (#is self)) (pos + len))
               )

        fun action_is f ({ match, len, follow, self, ...}: info) (k as LEX cont) pos =
          Cons (f (match, len, pos), lazy (fn () => #is self follow (LEX (#is
          self)) (pos + len)))

        val is_stdpn =
          action_is
            (fn (match, len, pos) =>
              ELEM ( PATH ( Node.create ( Symbol.fromValue (String.implode match)
                                   , (pos, pos + len)
                                   )
                          )
                   )
            )

        fun unfinished ({follow, ...} : info) _ pos =
          err
            ( LexError
                { reason = "unfinished mode"
                , pos = pos
                , rest = Stream.toList follow
                }
            )

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
            Cons (ELEM (STRING (Node.create (correct, (pos, pos')))),
                  lazy (fn () => cont follow' k pos'))
          end

        fun eof _ _ pos =
          Cons (EOF (pos, pos), eager Nil)

        fun error ({follow, ...}: info) _ pos =
          err
            ( LexError
                { reason = "illegal lexeme"
                , pos = pos
                , rest = Stream.toList follow
                }
            )

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

        fun unclosed_comment ({follow, ...}: info) pos =
          err
            ( LexError
                { reason = "unclosed comment"
                , pos = pos
                , rest = Stream.toList follow
                }
            )

        fun comment_error ({follow, ...}: info) pos =
          err
            ( LexError
                { reason = "illegal character in comment"
                , pos = pos
                , rest = Stream.toList follow
                }
            )

        fun unfinished_if ({follow, ...}: info) pos =
          err
            ( LexError
                { reason = "unfinished #if"
                , pos = pos
                , rest = Stream.toList follow
                }
            )

        fun error_if ({follow, ...}: info) pos =
          err
            ( LexError
                { reason = "illegal character in #if"
                , pos = pos
                , rest = Stream.toList follow
                }
            )

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

        fun unclosed_string ({follow, ...}: info) pos =
          err
            ( LexError
                { reason = "unclosed string"
                , pos = pos
                , rest = Stream.toList follow
                }
            )

        fun string_error ({follow, ...}: info) pos =
          err
            ( LexError
                { reason = "illegal character in string"
                , pos = pos
                , rest = Stream.toList follow
                }
            )

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
