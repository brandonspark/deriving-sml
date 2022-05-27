
structure Derive :
  sig
    val derive_exp : SMLSyntax.identifier -> SMLSyntax.ty -> SMLSyntax.exp

    val codegen_dec :
      SMLSyntax.dec * Context.t -> SMLSyntax.dec list * Context.t

    val codegen_spec :
      SMLSyntax.spec * Context.t -> SMLSyntax.spec list * Context.t
  end =
  struct
    open SMLSyntax


    infix |>
    fun x |> f = f x

    fun finalize_exp (pats, exp) =
      List.foldr
        (fn (pat, acc) =>
          Node.create_absurd (Efn [{pat = pat, exp = acc}])
        )
        exp
        pats

    fun derive_exp id ty =
      ( case id_to_string id of
          "show" => Show.from_ty ty
        | "eq" => Eq.from_ty ty
        | "cmp" => Compare.from_ty ty
        | "compare" => Compare.from_ty ty
        | "map" => Map.from_ty ty
        | _ => raise Fail "invalid deriving exp"
      )
      |> finalize_exp

    fun fix_derive (l, ctx) =
      (List.map Node.create_absurd l, ctx)

    fun codegen_dec (dec, ctx) =
      case Node.getVal dec of
        ( Dtype _
        | Ddatdec _
        | Ddatrepl _
        | Dabstype _ ) =>
            List.foldl
              (fn (f, (acc, ctx)) =>
                f (dec, ctx)
                |> (fn (decs, ctx) => (acc @ decs, ctx))
              )
              ([], ctx)
              [ Show.codegen_dec
              , Eq.codegen_dec
              , Compare.codegen_dec
              , Map.codegen_dec ]
            |> fix_derive
            |> (fn (decs, ctx) => (dec::decs, ctx))
      | ( Dval _
        | Dfun _
        | Dexception _
        | Dlocal _
        | Dopen _
        | Dempty
        | Dseq _
        | Dinfix _
        | Dinfixr _
        | Dnonfix _ ) =>
            ([dec], ctx)

    fun codegen_spec (spec, ctx) =
      case Node.getVal spec of
        ( SPtype _
        | SPeqtype _
        | SPdatdec _
        | SPdatrepl _ ) =>
            List.foldl
              (fn (f, (acc, ctx)) =>
                f (spec, ctx)
                |> (fn (specs, ctx) =>  (acc @ specs, ctx))
              )
              ([], ctx)
              [ Show.codegen_spec
              , Eq.codegen_spec
              , Compare.codegen_spec
              , Map.codegen_spec ]
            |> fix_derive
            |> (fn (specs, ctx) => (spec::specs, ctx))
      | ( SPval _
        | SPexception _
        | SPmodule _
        | SPinclude _
        | SPsharing _ ) =>
            ([spec], ctx)

  end
