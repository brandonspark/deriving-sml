
structure Eq : PLUGIN =
  struct
    open Prelude

    infix |>
    infix $

    fun long_ty_to_eq longid =
      case longid of
        [] => raise Fail "impossible, long_ty_to_eq on empty id"
      | _ =>
          change_last
            (fn node =>
              Node.map (fn sym => map_sym sym (fn s => s ^ "_eq")) node
            )
            longid

    infix &&
    fun x && y =
      Eandalso { left = x
               , right = y
               }
      |> promote

    fun ty_to_code get_tyvar_fn ty ctx (type_name : identifier option) =
      (* type_name should be SOME tycon if we are doing this for a type alias
       * named tycon.
       * otherwise it should be NONE
       *)
      let
        val id = new ()
        val id2 = new ()
        val pair = (Pident id, Pident id2)
        val epair = Etuple [Eident [id], Eident [id2]]

        fun prep_pair (pat1, pat2) = Ptuple [pat1, pat2]

        fun use_eq longid id =
          Eident (long_ty_to_eq longid) $ epair
      in
        case Node.getVal ty of
          SMLSyntax.Tident [ty_id] =>
            ( pair
            , (* Special cases for equaling any base types.
               *)
              case id_to_string ty_id of
                ( "int"
                | "string"
                | "char"
                | "bool"
                | "order" ) => Eident [mk_id "op="] $ epair
                | "real" =>
                  raise Fail "deriving eq for real"
                | _ =>
                    (* Look for the corresponding eq function.
                     *)
                    (case type_name of
                      SOME tycon =>
                        if id_eq (ty_id, tycon) then
                          raise Fail "type alias defined in terms of itself"
                        else
                          use_eq [ty_id] id
                    | NONE =>
                        use_eq [ty_id] id
                    )
            )

        (* Just use the eq function for a modular type.
         *)
        | SMLSyntax.Tident longty => (pair, use_eq longty id)

        (* Suppose we're codegen-ing for something like:
         * 'a
         * We don't know how to do equality for an `'a`.
         * But this codegen'd function simply assumes it's given such a
         * polymorphic equality function, referred to as `fni`, for the
         * tyvar's index.
         * (if we were deriving on an ('a, 'b) t, this index would be 0)
         *)
        | SMLSyntax.Ttyvar tyvar =>
            ( pair
            , get_tyvar_fn tyvar $ Etuple [Eident [id], Eident [id2]]
            )

        (* For polymorphic functions, get the right eq function and pass it the
         * eq functions for the instantiated tyargs.
         *)
        | SMLSyntax.Tapp (tys, longid) =>
            let
              (* For a list of types, get all the corresponding functions to
               * print them, and pass them to a desired expression.
               *)
              val codegen_tys =
                List.map
                  (fn ty => ty_to_code get_tyvar_fn ty ctx type_name)
                  tys

              fun add_helpers exp =
                List.foldl
                  (fn ((pats, exp), acc) =>
                    acc $ Efn [{pat = prep_pair pats, exp = exp}]
                  )
                  exp
                  codegen_tys
                $ epair

            in
              ( pair
              , case longid of
                  (* If it's not in a module, it may be `list` or `option`.
                   *)
                  [sing_id] =>
                    (case (codegen_tys, id_to_string sing_id) of
                      ([(pats, exp)], "list") =>
                        let
                          val list_eq_fn =
                              Eident [mk_id "ListPair", mk_id "allEq"]
                            $ Efn [{pat = prep_pair pats, exp = exp}]
                        in
                           list_eq_fn $ epair
                        end
                    | ([(pats, exp)], "option") =>
                        Ecase { exp = epair
                              , matches =
                                  [ (* (SOME x, SOME y) => f (x, y) *)
                                    { pat = Ptuple [ Papp { id = [mk_id "SOME"]
                                                          , atpat = Pident x
                                                          }
                                                   , Papp { id = [mk_id "SOME"]
                                                          , atpat = Pident y
                                                          }
                                                   ]
                                    , exp = Efn [{pat = prep_pair pats, exp = exp}]
                                          $ Etuple [Eident [x], Eident [y]]
                                    }
                                  , (* (NONE, NONE) => true *)
                                    { pat = Ptuple [ Pconstr (mk_id "NONE")
                                                   , Pconstr (mk_id "NONE")
                                                   ]
                                    , exp = Econstr (mk_id "true")
                                    }
                                  , (* _ => false *)
                                    { pat = promote Pwild
                                    , exp = Econstr (mk_id "false")
                                    }
                                  ]
                              }
                        |> promote
                    | (_, "list") => raise Fail "invalid number of tyargs to list"
                    | (_, "option") => raise Fail "invalid number of tyargs to option"
                    | _ => add_helpers (Eident (long_ty_to_eq longid))
                    )
                | _ => add_helpers (Eident (long_ty_to_eq longid))
              )
            end

        (* Functions are unprintable. *)
        | SMLSyntax.Tarrow (ty1, ty2) =>
            ((promote Pwild, promote Pwild), Econstr (mk_id "false"))

        (* For product types, codegen the components and combine.
         *)
        | SMLSyntax.Tprod tys =>
            let
              val codegen_tys =
                List.map
                  (fn ty => ty_to_code get_tyvar_fn ty ctx type_name)
                  tys

              val (pats1, pats2) =
                ListPair.unzip (List.map #1 codegen_tys)
            in
              ( (Ptuple pats1, Ptuple pats2)
              , List.foldl
                  (fn ((_, exp), NONE) => SOME exp
                  | ((_, exp), SOME acc) =>
                    SOME (acc && exp)
                  )
                  NONE
                  codegen_tys
                |> (fn NONE => raise Fail "empty product type in eq"
                   | SOME exp => exp
                   )
              )
            end

        (* For record types, codegen the components and combine.
         *)
        | SMLSyntax.Trecord fields =>
            let
              val codegen_tys =
                List.map
                  (fn {lab, ty} => (lab, ty_to_code get_tyvar_fn ty ctx type_name))
                  fields

              val (fields1, fields2) =
                List.map
                  (fn (lab, ((pat1, pat2), _)) => ((lab, pat1), (lab, pat2)))
                  codegen_tys
                |> ListPair.unzip

              fun mk_pats l =
                List.map
                  (fn (lab, pat) => PRlab {lab = lab, pat = pat})
                  l
                |> Precord
                |> promote
            in
              ( (mk_pats fields1, mk_pats fields2)
              , List.foldl
                  (fn ((lab, (_, exp)), NONE) =>
                      SOME exp
                  | ((lab, (_, exp)), SOME acc) =>
                      SOME (acc && exp)
                  )
                  NONE
                  codegen_tys
                |> (fn NONE => raise Fail "empty trecord in eq"
                   | SOME exp => exp)
              )
            end
      end

    fun from_ty ty =
      let
        val (fn_pats, get_tyvar_fn) = get_tyvar_info ty
      in
        ty_to_code
          get_tyvar_fn
          ty
          (* TODO: this only works because context is currently unused
           *)
          (Context.init)
          NONE
        |> (fn ((pat1, pat2), y) => (fn_pats @ [Ptuple[pat1, pat2]], y))
      end

    (* Check if a `deriving` actually has a `eq`
     *)
    fun verify_deriving deriving =
      case deriving of
        NONE => false
      | SOME plugins =>
          List.find
            (fn (id, _) => id_eq (id, mk_id "eq"))
            plugins
          |> (fn NONE => false
             | SOME (_, []) => true
             | SOME (_, _::_) => raise Fail "eq currently does not take options!"
             )

    (* Makes the function declarations for the eq functions.
     *)
    fun mk_fundec mk_init id =
      let
        val s = id_to_string id
      in
        [ mk_init (mk_id ("eq_" ^ s))
        , [ { opp = false
            , id = mk_id (s ^ "_eq")
            , pats = [Pident x]
            , ty = NONE
            , exp = Eident [mk_id ("eq_" ^ s)] $ Eident [x]
            }
          ]
        ]
      end

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
           *)
          fun add_constructor conid ((pat1, pat2), exp) =
            { pat = Ptuple [ Papp {id = [conid], atpat = pat1}
                           , Papp {id = [conid], atpat = pat2}
                           ]
            , exp = exp
            }

          (* These are the actual cases for the principal argument of the
           * function, which exhaustively handle each constructor.
           *)
          val matches =
            ( List.map
              (fn {id = conid, ty = tyopt, opp} =>
                case tyopt of
                  NONE =>
                    { pat = Ptuple [Pident conid, Pident conid]
                    , exp = Eident [mk_id "true"]
                    }
                | SOME ty =>
                    let
                      (* Not a `type` dec, so no need for the name. *)
                      val (pats, exp) = ty_to_code get_tyvar_fn ty ctx NONE
                    in
                      add_constructor conid (pats, exp)
                    end
              )
              conbinds
            )
            @
            (* Only add the wildcard case if there's more than 1 variant.
             *)
            ( case conbinds of
                [_] => []
               | _ => [ { pat = promote Pwild
                        , exp = Eident [mk_id "false"]
                        }
                      ]
            )


          (* The fvalbinds need to be made without going all the way to `Dfun`,
           * because they must be `and`ed together (in the same `Dfun`),
           * to support mutual recursion.
           *)
          fun mk_eq_fvalbind id =
            let
              val principal_arg = new ()
              val principal_arg2 = new ()
            in
              [ { opp = false
                , id = id
                , pats = fn_pats @ [Ptuple [ Pident principal_arg
                                           , Pident principal_arg2]
                                           ]
                , ty = NONE
                , exp =
                    Ecase { exp = Etuple [ Eident [principal_arg]
                                         , Eident [principal_arg2]
                                         ]
                          , matches = matches
                          }
                    |> promote
                }
              ]
            end
        in
          (* TODO?: can also make it just "eq" if it's called "t" *)
          mk_fundec mk_eq_fvalbind tycon
        end

    fun add_ctx ctx x = (x, ctx)

    fun codegen_dec (dec, ctx) =
      let
        fun finish_deriving fvalbinds =
          case fvalbinds of
            [] => []
          | _ => [ Dfun { tyvars = []
                        , fvalbinds = fvalbinds
                        }
                 ]
      in
        ( case Node.getVal dec of
          Ddatdec { datbinds, withtypee } =>
            (* Collect a list of all of the different function val binds, which
             * will then be placed in a single `Dfun` to `and` them together.
             *)
            List.foldl
              (fn (datbind, acc) => codegen_datbind datbind ctx @ acc)
              []
              datbinds
          |> (fn fvalbinds => finish_deriving fvalbinds
                (*
                (case withtypee of
                  ( NONE
                  | SOME typbinds ) =>
                *)
                (* TODO: I can't be arsed to add support for deriving withtypes.
                 * Just assume there's none for now.
                 *)
              )
        | Ddatrepl _ => [] (* TODO: add datrepl support *)
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
                    val ((pat1, pat2), exp) = ty_to_code get_tyvar_fn ty ctx (SOME tycon)

                    (* The fvalbinds need to be made without going all the way to `Dfun`,
                     * because they must be `and`ed together (in the same `Dfun`),
                     * to support mutual recursion.
                     *)
                    fun mk_eq_fvalbind id =
                      [ { opp = false
                        , id = id
                        , pats = fn_pats @ [Ptuple [pat1, pat2]]
                        , ty = NONE
                        , exp = exp
                        }
                      ]
                  in
                    mk_fundec mk_eq_fvalbind tycon
                  end
              )
              []
              typbinds
            |> finish_deriving
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
        ) |> add_ctx ctx
      end

    fun codegen_spec (spec, ctx) =
      let
        fun mk_funspec tycon eq_ty =
          [ SPval { id = mk_id ("eq_" ^ id_to_string tycon)
                  , ty = eq_ty
                  }
          , SPval { id = mk_id (id_to_string tycon ^ "_eq")
                  , ty = eq_ty
                  }
          ]

        fun pair_ty ty = promote (Tprod [ty, ty])
      in
        ( case Node.getVal spec of
          ( SPtype {tyvars, tycon, ty = _, deriving}
          | SPdatdec {tyvars, tycon, condescs = _ , deriving} ) =>
            if not (verify_deriving deriving) then
              []
            else
              let
                (* The types of all of the helper functions, for a polymorphic
                 * type.
                 *)
                val helper_tys =
                  List.map
                    (fn tyvar => Tarrow (pair_ty (Ttyvar tyvar), Tident [mk_id "bool"]))
                    tyvars

                (* The type we're generating eq for.
                 *)
                val self_ty =
                  case tyvars of
                    [] => Tident [tycon]
                  | _ => Tapp (List.map Ttyvar tyvars, [tycon])

                val eq_ty =
                  List.foldr
                    (fn (ty, acc) => Tarrow (ty, acc))
                    (Tarrow (pair_ty self_ty, Tident [mk_id "bool"]))
                    helper_tys
              in
                mk_funspec tycon eq_ty
              end
        | SPeqtype typdesc =>
            (* TODO: I can't be assed to support eqtype rn.
             *)
            []
        | SPdatrepl _ =>
            (* TODO: I can't be assed to support datrepl rn.
             *)
            []
        | ( SPexception _
          | SPmodule _
          | SPinclude _
          | SPsharing _
          | SPval _ ) => []
        ) |> add_ctx ctx
    end
  end
