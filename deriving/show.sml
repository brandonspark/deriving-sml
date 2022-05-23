
structure Show : DERIVING =
  struct
    open SMLSyntax

    infix |>
    fun x |> f = f x

    val id_of_str = fn x => Node.create (Symbol.fromValue x, Span.absurd)
    val x = id_of_str "x"

    val new = TempId.new

    fun map_sym sym f = Symbol.fromValue (f (Symbol.toValue sym))

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
              Node.map (fn sym => map_sym sym (fn s => s ^ "_show")) node
            )
            longid


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

    infix ^^
    fun x ^^ y =
      Eapp { left = Eident [id_of_str "op^"]
           , right = promote (Etuple [x, y])
           }

    val old_eif = Eif
    fun Eif x = promote (old_eif x)
    val old_efn = Efn
    fun Efn x = promote (old_efn x)



    fun ty_to_code get_tyvar_fn ty ctx (type_name : identifier option) =
      (* type_name should be SOME tycon if we are doing this for a type alias
       * named tycon.
       * otherwise it should be NONE
       *)
      (* TODO: add flag for recursion. should only be able to show_t if its
       * `type`, should err. but for `datatype`, its ok
       *)
      let
        val id = new ()

        fun mk_toString id = Eident [id_of_str id, id_of_str "toString"]

      in
        case Node.getVal ty of
          Tident [ty_id] =>
            ( Pident id
              (* Special cases for showing any base types.
               *)
            , case id_to_string ty_id of
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
                (case type_name of
                  SOME tycon =>
                    if id_eq (ty_id, tycon) then
                      raise Fail "type alias defined in terms of itself"
                    else
                      Eapp { left = Eident (long_ty_to_show [id])
                           , right = Eident [id]
                           }
                | NONE =>
                    (* is it the same as the thing we're currently deriving? *)
                    Eapp { left = Eident (long_ty_to_show [id])
                         , right = Eident [id]
                         }
                )
            )
        | Tident longty =>
            ( Pident id
            , Eapp { left = Eident (long_ty_to_show longty)
                   , right = Eident [id]
                   }
            )
        | Ttyvar tyvar =>
            (* Suppose we're codegen-ing for something like:
             * 'a
             * We don't know how to print an `'a`.
             * But this codegen'd function simply assumes it's given such a
             * polymorphic printing function, referred to as `fni`, for the
             * tyvar's index.
             * (if we were deriving on an ('a, 'b) t, this index would be 0)
             *)
            ( Pident id
            , Eapp { left = get_tyvar_fn tyvar
                   , right = Eident [id]
                   }
            )
        | Tapp (tys, longid) =>
            let
              val codegen_tys =
                List.map
                  (fn ty => ty_to_code get_tyvar_fn ty ctx type_name)
                  tys

              (* For a list of types, get all the corresponding functions to
               * print them, and pass them to a desired expression.
               *)
              fun add_printing_fns exp =
                Eapp { left = exp
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

            in
              ( Pident id
              , case longid of
                  [sing_id] =>
                    (* If it's not in a module, it may be `list` or `option`.
                     *)
                    (case (codegen_tys, id_to_string sing_id) of
                      ([(pat, exp)], "list") =>
                        let
                          val map_fn =
                            Eapp { left = Eident [mk_id "List", mk_id "map"]
                            , right = Efn [{pat = pat, exp = exp}]
                            }
                          val comma_separate =
                            Eapp { left = Eident [mk_id "String", mk_id "concatWith"]
                                 , right = Estring ", "
                                 }
                        in
                          (* "[" ^ String.concatWith ", " (List.map f id) ^ "]"
                           *)
                          Estring "["
                          ^^ Eapp { left = comma_separate
                                 , right =
                                     Eapp { left = map_fn
                                          , right = Eident [id]
                                          }
                                 }
                          ^^  Estring "]"
                        end
                    | ([(pat, exp)], "option") =>
                        Ecase { exp = Eident [id]
                              , matches =
                                  [ (* NONE => "NONE" *)
                                    { pat =
                                        promote (Pconstr {opp = false, id = [mk_id "NONE"]})
                                    , exp = Estring "NONE"
                                    }
                                  , (* SOME x => "SOME " ^ f x *)
                                    { pat =
                                        Papp { id = [mk_id "SOME"]
                                             , atpat = Pident (mk_id "x")
                                             }
                                    , exp =
                                           Estring "SOME "
                                        ^^ Eapp { left = Efn [{pat = pat, exp = exp}]
                                               , right = Eident [mk_id "x"]
                                               }
                                    }
                                  ]
                              }
                        |> promote
                    | (_, "list") => raise Fail "invalid number of tyargs to list"
                    | (_, "option") => raise Fail "invalid number of tyargs to option"
                    | _ => add_printing_fns (Eident (long_ty_to_show longid))
                    )
                | _ => add_printing_fns (Eident (long_ty_to_show longid))
                (* TODO: special case list, option *)
              )

            end
        | Tprod tys =>
            let
              val codegen_tys =
                List.map
                  (fn ty => ty_to_code get_tyvar_fn ty ctx type_name)
                  tys
            in
              ( promote (Ptuple (List.map #1 codegen_tys))
              , List.foldl
                  (fn ((_, exp), acc) =>
                    acc ^^ (Estring ", " ^^ exp)
                  )
                  (Estring "(")
                  codegen_tys
                |> (fn exp => exp ^^ Estring ")")
              )
            end
        | Tarrow (ty1, ty2) =>
            (* Functions are unprintable. *)
            (promote Pwild, Estring "<fn>")
        | Trecord fields =>
            let
              val codegen_tys =
                List.map
                  (fn {lab, ty} => (lab, ty_to_code get_tyvar_fn ty ctx type_name))
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
                    ^^ Estring ", "
                    ^^ promote (old_estring (Node.getVal lab))
                    ^^ Estring " = "
                    ^^ exp
                  )
                  (Estring "{")
                  codegen_tys
                |> (fn exp => exp ^^ Estring "}")
              )
            end
      end


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
            (fn (idx, _) => Pident (id_of_str ("fn" ^ Int.toString idx)))
            enum_tyvars
      in
        (fn_pats, get_tyvar_fn)
      end

    fun verify_deriving deriving =
      case deriving of
        NONE => false
      | SOME plugins =>
          List.find
            (fn (id, _) => id_eq (id, mk_id "show"))
            plugins
          |> (fn NONE => false
             | SOME (_, []) => true
             | SOME (_, _::_) => raise Fail "show currently does not take options!"
             )


    fun codegen_datbind {tyvars, tycon, conbinds, deriving} ctx =
      if not (verify_deriving deriving) then
        []
      else
        let
          (* Get the patterns and a function to obtain the right name from the
           * tyvars.
           *)
          val (fn_pats, get_tyvar_fn) = tyvars_to_fns tyvars

          (* To the pat and exp obtained, we need to add an application of the
           * principal constructor.
           * For instance, instead of (x, Int.toString x) for SOME, we need
           * (SOME x, "SOME " ^ Int.toString x)
           *)
          fun add_constructor conid (pat, exp) =
            { pat = Papp { id = [conid]
                         , atpat = pat
                         }
            , exp = Estring (id_to_string conid ^ " ")
                    ^^ exp
            }

          (* These are the actual cases for the principal argument of the
           * function, which exhaustively handle each constructor.
           *)
          val matches =
              List.map
              (fn {id = conid, ty = tyopt, opp} =>
                case tyopt of
                  NONE =>
                    { pat = promote Pwild, exp = Estring (id_to_string conid) }
                | SOME ty =>
                    let
                      (* Not a `type` dec, so no need for the name. *)
                      val (pat, exp) = ty_to_code get_tyvar_fn ty ctx NONE
                    in
                      add_constructor conid (pat, exp)
                    end
              )
              conbinds

          (* The fvalbinds need to be made without going all the way to `Dfun`,
           * because they must be `and`ed together (in the same `Dfun`),
           * to support mutual recursion.
           *)
          fun mk_show_fvalbind id =
            let
              val principal_arg = new ()
            in
              [ { opp = false
                , id = id_of_str id
                , pats = fn_pats @ [Pident principal_arg]
                , ty = NONE
                , exp =
                    Ecase { exp = Eident [principal_arg]
                          , matches = matches
                          }
                    |> promote
                }
              ]
            end
        in
          (* TODO?: can also make it just "show" if it's called "t" *)
          [ mk_show_fvalbind ("show_" ^ id_to_string tycon)
          , mk_show_fvalbind (id_to_string tycon ^ "_show")
          ]
        end


    fun codegen_dec dec ctx =
      case dec of
        Ddatdec { datbinds, withtypee } =>
          (* Collect a list of all of the different function val binds, which
           * will then be placed in a single `Dfun` to `and` them together.
           *)
          List.foldl
            (fn (datbind, acc) =>
              codegen_datbind datbind ctx @ acc
            )
            []
            datbinds
        |> (fn fvalbinds =>
              (*
              (case withtypee of
                ( NONE
                | SOME typbinds ) =>
              *)
              (* TODO: I can't be arsed to add support for deriving withtypes.
               * Just assume there's none for now.
               *)
              [ Dfun { tyvars = []
                   , fvalbinds = fvalbinds
                   }
              ]
            )
      | Ddatrepl _ => [dec] (* TODO: add datrepl support *)
      | Dtype typbinds =>
          List.foldl
            (fn ({tyvars, tycon, ty, deriving}, acc) =>
              if not (verify_deriving deriving) then
                []
              else
                let
                  val (fn_pats, get_tyvar_fn) = tyvars_to_fns tyvars

                  (* This is a typedec, so we do pass type_name as tycon.
                   *)
                  val (pat, exp) = ty_to_code get_tyvar_fn ty ctx (SOME tycon)

                  (* The fvalbinds need to be made without going all the way to `Dfun`,
                   * because they must be `and`ed together (in the same `Dfun`),
                   * to support mutual recursion.
                   *)
                  fun mk_show_fvalbind id =
                    [ { opp = false
                      , id = id_of_str id
                      , pats = fn_pats @ [pat]
                      , ty = NONE
                      , exp = exp
                      }
                    ]
                in
                  [ mk_show_fvalbind ("show_" ^ id_to_string tycon)
                  , mk_show_fvalbind (id_to_string tycon ^ "_show")
                  ]
                end
            )
            []
            typbinds
          |> ( fn fvalbinds =>
               [ Dfun { tyvars = []
                      , fvalbinds = fvalbinds
                      }
               ]
             )
      | Dabstype { datbinds, withtypee, withh } =>
          (* TODO: I can't be assed to support abstype rn.
           *)
          []
      | ( Dval _
        | Dfun _
        | Dexception _
        | Dlocal _
        | Dopen _
        | Dempty
        | Dseq _
        | Dinfix _
        | Dinfixr _
        | Dnonfix _ ) => []

    val old_tident = Tident
    fun Tident x = promote (old_tident x)
    val old_tapp = Tapp
    fun Tapp x = promote (old_tapp x)
    val old_ttyvar = Ttyvar
    fun Ttyvar x = promote (old_ttyvar x)
    val old_tarrow = Tarrow
    fun Tarrow x = promote (old_tarrow x)

    fun codegen_spec spec ctx =
      case spec of
        SPtype {tyvars, tycon, ty = tyopt, deriving} =>
          if not (verify_deriving deriving) then
            []
          else
            let
              (* The type we're generating show for.
               *)
              val self_ty =
                case tyvars of
                  [] => Tident [tycon]
                | _ => Tapp (List.map Ttyvar tyvars, [tycon])

              val show_ty =
                (Tarrow (self_ty, Tident [mk_id "string"]))
            in
              [ SPval { id = mk_id ("show_ " ^ id_to_string tycon)
                      , ty = show_ty
                      }
              , SPval { id = mk_id (id_to_string tycon ^ "_show")
                      , ty = show_ty
                      }
              ]
            end
      | SPdatdec {tyvars, tycon, condescs, deriving} =>
          if not (verify_deriving deriving) then
            []
          else
            let
              (* The types of all of the helper functions, for a polymorphic
               * type.
               *)
              val helper_tys =
                List.map
                  (fn tyvar => Tarrow (Ttyvar tyvar, Tident [mk_id "string"]))
                  tyvars

              (* The type we're generating show for.
               *)
              val self_ty =
                case tyvars of
                  [] => Tident [tycon]
                | _ => Tapp (List.map Ttyvar tyvars, [tycon])

              val show_ty =
                List.foldr
                  (fn (ty, acc) => Tarrow (ty, acc))
                  (Tarrow (self_ty, Tident [mk_id "string"]))
                  helper_tys
            in
              [ SPval { id = mk_id ("show_ " ^ id_to_string tycon)
                      , ty = show_ty
                      }
              , SPval { id = mk_id (id_to_string tycon ^ "_show")
                      , ty = show_ty
                      }
              ]
            end
      | SPeqtype typdesc =>
          (* TODO: I can't be assed to support eqtype rn.
           *)
          []
      | SPdatrepl _ =>
          (* I can't be assed to support datrepl rn.
           *)
          []
      | ( SPexception _
        | SPmodule _
        | SPinclude _
        | SPsharing _
        | SPval _ ) => []
  end
