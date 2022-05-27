
(* PLUGIN is the signature for a deriving plugin.
 * It produces a certain number of:
 * - declarations
 * - specifications
 * from deriving annotations.
 * It also details how to produce an in-place expression from a deriving thing.
 *)

signature PLUGIN =
  sig

    (* Both of these are used during the program transformation stage.
     *)
    val codegen_dec : SMLSyntax.dec * Context.t -> SMLSyntax.dec_ list * Context.t
    val codegen_spec : SMLSyntax.spec * Context.t -> SMLSyntax.spec_ list * Context.t

    (* Need to expose this, because we use this at the rewriting step during
     * parsing.
     *)
    val from_ty : SMLSyntax.ty -> SMLSyntax.pat list * SMLSyntax.exp

  end
