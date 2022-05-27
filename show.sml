val _ = Control.Print.printDepth := 1000
local
  fun open_source filename =
      let
        val instream = (TextIO.openIn ( filename ))
      in
        (Source.newSource ( (filename,
                             instream,
                             false,
                             (ErrorMsg.defaultConsumer ( () ))) ))
      end
in
  fun mk_ast filename =
      (SmlFile.parse ( (open_source ( filename )) ))
end
structure Find =
  struct
    open Ast
    fun foldl_exp f z exp = (case exp of (VarExp p) => f )
  end
