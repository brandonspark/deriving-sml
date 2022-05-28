
structure Top :
  sig
    val run : unit -> unit
  end =
  struct

    infix |>
    fun x |> f = f x

    open Error

    structure StrSet = StringRedBlackSet

    (* Makes fresh filenames *)
    fun mk_new_filename filename =
        case OS.Path.dir filename of
          "" => "derived_" ^ OS.Path.file filename
        | other => other ^ "/derived_" ^ OS.Path.file filename

    (* Swaps the contents of two files *)
    fun swaperoo ast filename new_filename =
      let
        val _ = OS.Process.system ("touch " ^ new_filename)

        val command = "cp " ^ filename ^ " " ^ new_filename
        (* Copy the original file to the new name *)
        val _ = OS.Process.system command

        (* Open the original file and write the derived AST into it *)
        val outstream = TextIO.openOut filename
        val _ = TextIO.output (outstream, PrettyPrintAst.pretty ast false)
        val _ = TextIO.closeOut outstream
      in
        ()
      end

    fun swap_back original_filename new_filename =
      let
        val command = "mv " ^ new_filename ^ " " ^ original_filename
      in
        OS.Process.system command
      end

    fun derive_file filename =
      ( case Parser.parse_file_transformed filename of
          Either.INL strings =>
            err (ParseError (filename, strings))
        | Either.INR (ast, []) =>
            SOME (filename, ast, mk_new_filename filename)
        | Either.INR (ast, toks) =>
            err (ParseError (filename, List.map Token.token_to_string toks))
      )
      handle
        Signal (SigError error) =>
          (case error of
            ParseError info => (warn () (ParseWarning info))
          | TransformError {reason, pos} =>
              ( warn ()
                (TransformWarning
                  {filename = filename, reason = reason, pos = pos}
                )
              )
          | LexError {reason, pos, rest} =>
              ( warn ()
                (LexWarning
                  {filename = filename, reason = reason, pos = pos, rest = rest}
                )
              )
          | ExpectedIdent {expected, got, span} =>
              (warn ()
                (GeneralWarning
                  { filename = filename
                  , reason = "Expected ident " ^ expected ^ ", but got " ^ got
                  , span = span
                  }
                )
              )
          | FixityError {reason, span} =>
              (warn ()
                (GeneralWarning
                  {filename = filename, reason = reason, span=span}
                )
              )
          ; NONE
          )
      | Overflow => NONE
      (* TODO: Should handle more errors here *)


    (* must be called with canonical paths *)
    fun collect_paths cur_path filename seen =
      let
        val canonical_new =
          OS.Path.mkCanonical (OS.Path.concat (cur_path, filename))
      in
        if StrSet.member seen canonical_new then
          ([], seen)
        else
          let
            val seen = StrSet.insert seen canonical_new

            val (new_dir, new_path) =
              if OS.Path.isAbsolute filename then
                (OS.Path.dir filename, filename)
              else
                ( OS.Path.dir canonical_new
                , canonical_new
                )
          in
            case (String.sub (filename, 0), OS.Path.ext filename) of
              (_, NONE) =>
                warn ([], seen) (InvalidFile filename)
            | (#"$", _) => ([], seen)
            | (_, SOME ("sml" | "sig" | "fun")) => ([new_path], seen)
            | (_, SOME "cm") =>
                (case OS.Path.file filename of
                  "cmlib.cm" => ([], seen)
                | _ =>
                  (case CM_Parser.parse_file new_path of
                    Either.INL strs =>
                      warn ([], seen) (ParseWarning (filename, strs))
                  | Either.INR (elems, []) =>
                      List.foldl
                        (fn (tok, (paths, seen)) =>
                          let
                            val (new_paths, seen) =
                              collect_paths' new_dir seen tok
                          in
                            (new_paths @ paths, seen)
                          end
                        )
                        ([], seen)
                        elems
                  | Either.INR (elems, rest) =>
                      warn ([], seen)
                        (ParseWarning (filename, List.map CM_Token.token_to_string rest))
                  )
                )
            | _ => warn ([], seen) (InvalidExt filename)
          end
      end

    and collect_paths' new_path seen tok =
      case tok of
        CM_Token.PATH node =>
          collect_paths
            new_path
            (OS.Path.mkCanonical (Symbol.toValue (Node.getVal node)))
            seen
      | CM_Token.STRING node =>
          collect_paths
            new_path
            (OS.Path.mkCanonical (Node.getVal node))
            seen

    fun run () =
      let
        val args = CommandLine.arguments ()
      in
        case args of
          [] => ()
        | filenames =>
            let
              val files =
                List.foldl
                  (fn (filename, (paths, seen)) =>
                    let
                      val (new_paths, seen) =
                        collect_paths "" (OS.Path.mkCanonical filename) seen
                    in
                      (new_paths @ paths, seen)
                    end
                  )
                  ([], StrSet.empty)
                  filenames
                |> (fn (paths, _) => paths)

              val file_info = List.mapPartial derive_file files

              val _ = print "asts achieved\n"
            in
              ( List.map
                  (fn (old_name, ast, new_name) => swaperoo ast old_name new_name)
                  file_info
              ; OS.Process.system ("sml " ^ String.concatWith " " args)
              ; List.map (fn (old_name, _, new_name) =>
                  swap_back old_name new_name
                ) file_info
              ; OS.Process.exit 0
              )
            end
      end
  end

val _ = Top.run ()
