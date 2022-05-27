
structure Top =
  struct
    fun mk_derived_file ast filename =
      let
        val derived_filename = OS.Path.base filename ^ "_derive15150.sml"
        val outstream = TextIO.openOut derived_filename
        val _ = TextIO.output (outstream, PrettyPrintAst.pretty ast false)
        val _ = TextIO.closeOut outstream
      in
        derived_filename
      end

    fun derive_file filename =
      case Parser.parse_file_transformed filename of
        Either.INL _ =>
          raise Fail ("failed to parse/transform file " ^ filename)
      | Either.INR (ast, _::_) =>
          raise Fail ("there are unparsed tokens:" ^ filename)
      | Either.INR (ast, []) =>
        let
          val derived_filename = mk_derived_file ast filename
        in
          ( OS.Process.system ("sml " ^ derived_filename)
          ; OS.Process.system ("rm " ^ derived_filename)
          ; OS.Process.exit 0
          )
        end


    fun collect_paths cur_path filename =
      let
        val (new_dir, new_path) =
          if OS.Path.isAbsolute filename then
            (OS.Path.dir filename, filename)
          else
            ( OS.Path.concat (cur_path, OS.Path.dir filename)
            , OS.Path.concat (cur_path, filename)
            )
        val _ = print ("path is now " ^ cur_path ^ "\n")
      in
        case (String.sub (filename, 0), OS.Path.ext filename) of
          (_, NONE) => raise Fail ("expected filename, got " ^ new_path)
        | (#"$", _) => []
        | (_, SOME ("sml" | "sig" | "fun")) => [new_path]
        | (_, SOME "cm") =>
            (case CM_Parser.parse_file new_path of
              Either.INL _ => raise Fail ("failed to parse cm file " ^ new_path)
            | Either.INR (elems, []) =>
                List.concatMap (collect_paths' new_dir) elems
            | Either.INR (elems, _) =>
                raise Fail "incomplete cm parse"
            )
        | _ => raise Fail "unrecognized extension from derived file"
      end

    and collect_paths' new_path tok =
      case tok of
        CM_Token.PATH node => collect_paths new_path (Symbol.toValue (Node.getVal node))
      | CM_Token.STRING node => collect_paths new_path (Node.getVal node)

    val collect_paths = fn filename =>
      List.map OS.Path.mkCanonical (collect_paths "" filename)

    fun run () =
      let
        val args = CommandLine.arguments ()
      in
        case args of
          [] => ()
        | filenames => ()
      end
  end

val _ = Top.run ()
