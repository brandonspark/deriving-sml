
structure Prelude =
  struct
    open SMLSyntax

    infix |>
    fun x |> f = f x

    val x = mk_id "x"
    val y = mk_id "y"

    val new = TempId.new

    fun change_last f l =
      case l of
        [] => raise Fail "changing last on empty list"
      | [x] => [f x]
      | x::xs =>
          let
            val res = change_last f xs
          in
            x::res
          end

    local
      fun split [] = ([], [])
        | split [x] = ([x], [])
        | split (x::y::xs) =
          let
            val (A, B) = split xs
          in
            (x::A, y::B)
          end

      fun merge cmp ([], B) = B
        | merge cmp (A, []) = A
        | merge cmp (x::L, y::R) =
          case cmp (x, y) of
            LESS => x :: merge cmp (L, y :: R)
          | _ => y :: merge cmp (x :: L, R)
    in
      fun msort cmp [] = []
        | msort cmp [x] = [x]
        | msort cmp L =
          let
            val (A, B) = split L
          in
            merge cmp (msort cmp A, msort cmp B)
          end
    end

    fun gather_tyvars ty =
      case Node.getVal ty of
        SMLSyntax.Tident _ => []
      | SMLSyntax.Ttyvar id => [id]
      | SMLSyntax.Tapp (tys, _) => List.concatMap gather_tyvars tys
      | SMLSyntax.Tprod tys => List.concatMap gather_tyvars tys
      | SMLSyntax.Tarrow (ty1, ty2) => gather_tyvars ty1 @ gather_tyvars ty2
      | SMLSyntax.Trecord fields => List.concatMap (gather_tyvars o #ty) fields

    fun promote x = Node.create (x, Span.absurd)

    val old_pident = Pident
    fun Pident id = promote (old_pident {opp = false, id = id})
    val old_eident = Eident
    fun Eident ids = promote (old_eident {opp = false, id = ids})
    val old_estring = Estring
    fun Estring id = promote (old_estring (Symbol.fromValue id))
    val old_eapp = Eapp
    fun Eapp {left, right} =
      promote (old_eapp {left = left, right = right})
    val old_papp = Papp
    fun Papp x = promote (old_papp x)
    val old_pconstr = Pconstr
    fun Pconstr x = promote (old_pconstr {opp = false, id = [x]})
    val old_ptuple = Ptuple
    fun Ptuple x = promote (old_ptuple x)

    val old_eif = Eif
    fun Eif x = promote (old_eif x)
    val old_efn = Efn
    fun Efn x = promote (old_efn x)
    val old_etuple = Etuple
    fun Etuple x = promote (old_etuple x)
    val old_econstr = Econstr
    fun Econstr x = promote (old_econstr {opp = false, id = [x]})

    infix $
    fun x $ y =
      Eapp { left = x
           , right = y
           }

    val identity = Efn [ {pat = Pident x, exp = Eident [x]} ]

    val old_tident = Tident
    fun Tident x = promote (old_tident x)
    val old_tapp = Tapp
    fun Tapp x = promote (old_tapp x)
    val old_ttyvar = Ttyvar
    fun Ttyvar x = promote (old_ttyvar x)
    val old_tarrow = Tarrow
    fun Tarrow x = promote (old_tarrow x)

    (* This function associates the tyvars to the desired patterns, indexed by
     * tyvar, and then also a function which produces the right name from the
     * right tyvar.
     *)
    fun tyvars_to_fns tyvars =
      let
        val enum_tyvars = List.mapi Fn.id tyvars

        (* This function associates a tyvar to an index in the datatype *)
        fun get_tyvar_fn tyvar =
          case
            List.find (fn (idx, tyvar') =>
              Node.location_insensitive_eq Symbol.eq (tyvar, tyvar')
            ) enum_tyvars
          of
            NONE => raise Fail "unable to print tyvar"
          | SOME (idx, _) =>
              Eident [mk_id ("fn" ^ Int.toString idx)]

        (* All of the patterns we'll need for the function clause.
         * fun f fn1 fn2 fn3 ... =
         *)
        val fn_pats =
          List.map
            (fn (idx, _) => Pident (mk_id ("fn" ^ Int.toString idx)))
            enum_tyvars
      in
        (fn_pats, get_tyvar_fn)
      end

    fun get_tyvar_info ty =
      gather_tyvars ty
      |> msort
          (fn (n1, n2) => Symbol.compare (Node.getVal n1, Node.getVal n2))
      |> tyvars_to_fns




  end
