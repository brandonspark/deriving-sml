
(* DERIVING is the signature for a deriving plugin.
 * It produces a certain number of:
 * - declarations
 * - specifications
 * from deriving annotations.
 *)

signature DERIVING =
  sig

    val codegen_dec : SMLSyntax.dec_ -> Context.t -> SMLSyntax.dec_ list
    val codegen_spec : SMLSyntax.spec_ -> Context.t -> SMLSyntax.spec_ list

  end
