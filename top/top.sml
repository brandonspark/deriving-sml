
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
        Either.INL _ => raise Fail "failed to derive!"
      | Either.INR (ast, _::_) => raise Fail "there are unparsed tokens"
      | Either.INR (ast, []) =>
        let
          val derived_filename = mk_derived_file ast filename
        in
          ( OS.Process.system ("sml " ^ derived_filename)
          (* ; OS.Process.system ("rm " ^ derived_filename) *)
          ; OS.Process.exit 0
          )
        end


  end

