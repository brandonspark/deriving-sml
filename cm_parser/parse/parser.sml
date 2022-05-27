
type result = CM_Token.elem list * CM_Token.token list
signature PARSER =
  sig
    val parse : char StreamStreamable.t -> (string list, result) Either.either

    val parse_string : string -> (string list, result) Either.either

    val parse_file : string -> (string list, result) Either.either

    val parse_file_to_string : string -> string
  end

structure CM_Parser :> PARSER =
  struct
    type span = Span.span

    structure S = Stream

    type symbol = Symbol.symbol

    fun identity x = x
    fun null () = []
    fun sing x = [x]
    fun pair (x, y) = [x, y]
    fun sing_span (x, span) = Node.create

    val option_to_bool = fn
      SOME _ => true
    | NONE => false

    fun nyi () = ()
    fun assert_fvalbinds_valid _ = nyi ()
    fun assert_valbinds_valid _ _ = nyi ()
    fun assert_valid_precedence _ = nyi ()

    structure Arg =
      struct
        datatype terminal = datatype CM_Token.token

        datatype elem = datatype CM_Token.elem

        type identifier = symbol Node.t
        type span = span

        type files = elem list

        val nil_files = null
        val cons_files = op::

        type main = files
        val main_prog = identity

        exception Error of CM_Token.token StreamStreamable.t
        fun error x = Error x
      end

    (* Sidestepping the NJ extension so it can parse itself. *)
    structure Input =
      struct
        structure Streamable = StreamStreamable
        structure Arg = Arg
      end

    structure ParseMain =
      ParserFun (Input)

    fun parse cs =
      let
        val (elems, stream) = ParseMain.parse (CM_Lexer.lex cs)
      in
        Either.INR (elems, Stream.toList stream)
      end
        handle Arg.Error x => Either.INL (List.map CM_Token.token_to_string
        (Stream.toList x))

    fun parse_string s = parse (Stream.fromList (String.explode s))

    fun parse_file s =
      let
        val instream = TextIO.openIn s
        val input = TextIO.inputAll instream
      in
        parse_string input
      end

    fun parse_file_to_string s =
      case parse_file s of
        Either.INL _ => raise Fail "Failed to parse!"
      | Either.INR (elems, _) =>
          String.concatWith " " (List.map CM_Token.elem_to_string elems)
  end
