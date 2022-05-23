
(* DERIVING is the signature for a deriving plugin.
 * It produces a certain number of:
 * - declarations
 * - specifications
 * from deriving annotations.
 *)

signature DERIVING =
  sig

    val codegen_dec : SMLSyntax.dec * Context.t -> SMLSyntax.dec_ list * Context.t
    val codegen_spec : SMLSyntax.spec * Context.t -> SMLSyntax.spec_ list * Context.t
    val from_ty : SMLSyntax.ty -> SMLSyntax.pat * SMLSyntax.exp

  end
