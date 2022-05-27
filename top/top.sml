
structure Top =
  struct
    (* Makes fresh filenames *)
    fun mk_new_filename filename =
      if OS.Process.isSuccess (OS.Process.system ("test -f " ^ filename)) then
        mk_new_filename ("new_" ^ filename)
      else
        filename

    (* Swaps the contents of two files *)
    fun swaperoo ast filename new_filename =
      let
        (* Copy the original file to the new name *)
        val _ = OS.Process.system ("cp " ^ filename ^ " " ^ new_filename)

        (* Open the original file and write the derived AST into it *)
        val outstream = TextIO.openOut filename
        val _ = TextIO.output (outstream, PrettyPrintAst.pretty ast false)
        val _ = TextIO.closeOut outstream
      in
        ()
      end

    fun swap_back original_filename new_filename =
      OS.Process.system ("mv " ^ new_filename ^ " " ^ original_filename)

    fun derive_file filename =
      case Parser.parse_file_transformed filename of
        Either.INL _ =>
          raise Fail ("failed to parse/transform file " ^ filename)
      | Either.INR (ast, _::_) =>
          raise Fail ("there are unparsed tokens:" ^ filename)
      | Either.INR (ast, []) =>
          (filename, ast, mk_new_filename filename)


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
        | filenames =>
            let
              val files = List.concatMap collect_paths filenames

              val file_info = List.map derive_file files
            in
              ( List.map
                  (fn (old_name, ast, new_name) => swaperoo ast old_name new_name)
                  file_info
              ; OS.Process.system ("rlwrap sml " ^ String.concatWith " " args)
              ; List.map (fn (old_name, _, new_name) =>
                  swap_back old_name new_name
                ) file_info
              ; OS.Process.exit 0
              )
            end
      end
  end

val _ = Top.run ()
