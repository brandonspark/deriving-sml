
structure Show : DERIVING =
  struct
    open SMLSyntax

    infix |>
    fun x |> f = f x

    val id_of_str = fn x => Node.create (Symbol.fromValue x, Span.absurd)
    val x = id_of_str "x"

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

    fun long_ty_to_show longid =
      case longid of
        [] => raise Fail "impossible, long_ty_to_show on empty id"
      | _ =>
          change_last
            (fn node =>
              Node.map (fn sym => Symbol.fromValue (Symbol.toValue sym ^ "_show")) node
            )
            longid

    fun ty_to_code tyvar_enum_fn ty ctx =
      (* TODO: add flag for recursion. should only be able to show_t if its
       * `type`, should err. but for `datatype`, its ok
       *)
      let
        val id = new ()
        fun tyvar_idx_to_fn idx = id_of_str ("fn" ^ Int.toString idx)

        fun promote x = Node.create (x, Span.absurd)

        val old_pident = Pident
        fun Pident id = promote (old_pident {opp = false, id = id})
        val old_eident = Eident
        fun Eident id = promote (old_eident {opp = false, id = id})
        val old_estring = Estring
        fun Estring id = promote (old_estring (Symbol.fromValue id))
        val old_eapp = Eapp
        fun Eapp {left, right} =
          promote (old_eapp {left = left, right = right})

        fun x ^ y =
          Eapp { left = Eident [id_of_str "op^"]
               , right = promote (Etuple [x, y])
               }

        val old_eif = Eif
        fun Eif x = promote (old_eif x)
        val old_efn = Efn
        fun Efn x = promote (old_efn x)

        fun mk_toString id = Eident [id_of_str id, id_of_str "toString"]

      in
        case Node.getVal ty of
          Tident [ty_id] =>
            ( Pident id
            , case Symbol.toValue (Node.getVal ty_id) of
              "int" =>
                Eapp { left = mk_toString "Int"
                     , right = Eident [id]
                     }
            | "string" =>
                Eident [id]
            | "real" =>
                Eapp { left = mk_toString "Real"
                     , right = Eident [id]
                     }
            | "char" =>
                Eapp { left = mk_toString "Char"
                     , right = Eident [id]
                     }
            | "bool" =>
                Eif { exp1 = Eident [id]
                    , exp2 = Estring "true"
                    , exp3 = Estring "false"
                    }
            | _ =>
                (* is it the same as the thing we're currently deriving? *)
                Eapp { left = Eident (long_ty_to_show [id])
                     , right = Eident [id]
                     }
            )
        | Ttyvar id =>
            (* Suppose we're codegen-ing for something like:
             * 'a
             * We don't know how to print an `'a`.
             * But this codegen'd function simply assumes it's given such a
             * polymorphic printing function, referred to as `fni`, for the
             * tyvar's index.
             * (if we were deriving on an ('a, 'b) t, this index would be 0)
             *)
            let
              val tyvar_id = tyvar_enum_fn id
            in
              ( Pident id
              , Eapp { left = Eident [tyvar_idx_to_fn tyvar_id]
                     , right = Eident [id]
                     }
              )
            end
        | Tapp (tys, longid) =>
            let
              val codegen_tys = List.map (fn ty => ty_to_code tyvar_enum_fn ty ctx) tys
            in
              (* TODO: special case list, option *)
              ( Pident id
              , Eapp { left = Eident (long_ty_to_show longid)
                     , right =
                       List.foldl
                        (fn ((pat, exp), acc) =>
                          Eapp { left = acc
                               , right = Efn [{pat = pat, exp = exp}]
                               }
                        )
                        (Eident [id])
                        codegen_tys
                     }
              )
            end
        | Tprod tys =>
            let
              val codegen_tys = List.map (fn ty => ty_to_code tyvar_enum_fn ty ctx) tys
            in
              ( promote (Ptuple (List.map #1 codegen_tys))
              , List.foldl
                  (fn ((_, exp), acc) =>
                    acc ^ (Estring ", " ^ exp)
                  )
                  (Estring "(")
                  codegen_tys
                |> (fn exp => exp ^ Estring ")")
              )
            end
        | Tarrow (ty1, ty2) =>
            (* Functions are unprintable. *)
            (promote Pwild, Estring "<fn>")
        | Trecord fields =>
            let
              val codegen_tys =
                List.map
                  (fn {lab, ty} => (lab, ty_to_code tyvar_enum_fn ty ctx))
                  fields
            in
              ( promote
                  ( Precord
                      (List.map
                        (fn (lab, (pat, _)) => PRlab {lab = lab, pat = pat})
                        codegen_tys
                      )
                  )
              , List.foldl
                  (fn ((lab, (_, exp)), acc) =>
                      acc
                    ^ Estring ", "
                    ^ promote (old_estring (Node.getVal lab))
                    ^ Estring " = "
                    ^ exp
                  )
                  (Estring "{")
                  codegen_tys
                |> (fn exp => exp ^ Estring "}")
              )
            end
      end





(*

    fun codegen_datbind {tyvars, tycon, conbinds, deriving} ctx =
      case deriving of
        NONE => []
      | SOME plugins =>
          let
            val num_tyvars = List.length tyvars
          in

          end


    fun codegen_dec dec ctx =
      case dec of
        Ddatdec { datbinds, withtypee } =>
          List.foldl
            (fn (datbind, acc) =>
                  codegen_datbind datbind plugins ctx @ acc
            )
            []
            datbinds
        |>


      | Ddatrepl _ => [dec] (* TODO: add datrepl support *)
      | Dtype typbinds =>
      | Dabstype => { datbinds, withtypee, withh } =>
      | ( Dval _
        | Dfun _
        | Dexception _
        | Dlocal _
        | Dopen _
        | Dempty _
        | Dseq _
        | Dinfix _
        | Dinfixr _
        | Dnonfix _ ) =>
      *)

  end
