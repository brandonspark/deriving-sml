
structure Compare : PLUGIN =
  struct
    open Prelude

    infix |>
    infix $

    fun long_ty_to_cmp longid =
      case longid of
        [] => raise Fail "impossible, long_ty_to_cmp on empty id"
      | _ =>
          change_last
            (fn node =>
              Node.map (fn sym => map_sym sym (fn s => s ^ "_compare")) node
            )
            longid

    infix >>=
    fun exp >>= cont =
      Ecase { exp = exp
            , matches =
                [ { pat = Pconstr (mk_id "LESS")
                  , exp = Econstr (mk_id "LESS")
                  }
                , { pat = Pconstr (mk_id "GREATER")
                  , exp = Econstr (mk_id "GREATER")
                  }
                , { pat = promote Pwild
                  , exp = cont
                  }
                ]
            }
      |> promote

    val int_compare = Eident [mk_id "Int", mk_id "compare"]

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

        fun mk_compare id = Eident [mk_id id, mk_id "compare"]

        fun use_cmp longid id =
          Eident (long_ty_to_cmp longid) $ epair
      in
        case Node.getVal ty of
          SMLSyntax.Tident [ty_id] =>
            ( pair
            , (* Special cases for comparing any base types.
               *)
              case id_to_string ty_id of
                  "int" => mk_compare "Int" $ epair
                | "string" => mk_compare "String" $ epair
                | "char" => mk_compare "Char" $ epair
                | "bool" =>
                    (Ecase { exp = epair
                           , matches =
                             [ { pat = Ptuple
                                         [Pconstr (mk_id "true"), Pconstr (mk_id "false")]
                               , exp = Econstr (mk_id "GREATER")
                               }
                             , { pat = Ptuple [ Pconstr (mk_id "false")
                                              , Pconstr (mk_id "true")
                                              ]
                               , exp = Econstr (mk_id "LESS")
                               }
                             , { pat = promote Pwild
                               , exp = Econstr (mk_id "EQUAL")
                               }
                             ]
                           }
                    ) |> promote
                | "order" =>
                    let
                      val enum_exp =
                        Efn [ { pat = Pconstr (mk_id "LESS")
                              , exp = Enumber (Int 0) |> promote
                              }
                            , { pat = Pconstr (mk_id "EQUAL")
                              , exp = Enumber (Int 1) |> promote
                              }
                            , { pat = Pconstr (mk_id "GREATER")
                              , exp = Enumber (Int 2) |> promote
                              }
                            ]
                    in
                      int_compare $ Etuple [ enum_exp $ Eident [id]
                                           , enum_exp $ Eident [id2]
                                           ]
                    end
                | "real" => mk_compare "Real" $ epair
                | _ =>
                    (* Look for the corresponding cmp function.
                     *)
                    (case type_name of
                      SOME tycon =>
                        if id_eq (ty_id, tycon) then
                          raise Fail "type alias defined in terms of itself"
                        else
                          use_cmp [ty_id] id
                    | NONE =>
                        use_cmp [ty_id] id
                    )
            )

        (* Just use the cmp function for a modular type.
         *)
        | SMLSyntax.Tident longty => (pair, use_cmp longty id)

        (* Suppose we're codegen-ing for something like:
         * 'a
         * We don't know how to do comparison for an `'a`.
         * But this codegen'd function simply assumes it's given such a
         * polymorphic comparison function, referred to as `fni`, for the
         * tyvar's index.
         * (if we were deriving on an ('a, 'b) t, this index would be 0)
         *)
        | SMLSyntax.Ttyvar tyvar =>
            ( pair
            , get_tyvar_fn tyvar $ epair
            )

        (* For polymorphic functions, get the right comparison function and pass it the
         * cmp functions for the instantiated tyargs.
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
                    (fn ((pats, exp), acc) => acc $ Efn [{pat = prep_pair pats, exp = exp}])
                    exp
                    codegen_tys
                $ epair

              val list_length = Eident [mk_id "List", mk_id "length"]
            in
              ( pair
              , case longid of
                  (* If it's not in a module, it may be `list` or `option`.
                   *)
                  [sing_id] =>
                    (case (codegen_tys, id_to_string sing_id) of
                      ([(pats, exp)], "list") =>
                        int_compare $
                          Etuple [ list_length $ Eident [id]
                                 , list_length $ Eident [id2]
                                 ]
                        >>=
                        (* List.foldl
                         *   (fn ((elem1, elem2), acc) =>
                         *     case acc of
                         *       LESS => LESS
                         *     | GREATER => GREATER
                         *     | EQUAL => f (elem1, elem2)
                         *   )
                         *   EQUAL
                         *   (ListPair.zip (id1, id2))
                         *)
                        ( Eident [mk_id "List", mk_id "foldl"]
                        $ Efn [ { pat =
                                    Ptuple
                                      [ Ptuple
                                        [ Pident (mk_id "elem1")
                                        , Pident (mk_id "elem2")
                                        ]
                                      , Pident (mk_id "acc")
                                      ]
                                , exp =
                                    Eident [mk_id "acc"]
                                    >>=
                                    ( Efn [ { pat = prep_pair pats
                                            , exp = exp
                                            }
                                          ]
                                    $ Etuple
                                        [ Eident [mk_id "elem1"]
                                        , Eident [mk_id "elem2"]
                                        ]
                                    )
                                }
                              ]
                        $ Eident [mk_id "EQUAL"]
                        $ (Eident [mk_id "ListPair", mk_id "zip"] $ epair)
                        )
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
                                  , (* (NONE, SOME _) => LESS *)
                                    { pat = Ptuple [ Pconstr (mk_id "NONE")
                                                   , Papp { id = [mk_id "SOME"]
                                                          , atpat = promote Pwild
                                                          }
                                                   ]
                                    , exp = Eident [mk_id "LESS"]
                                    }
                                  , (* (SOME _, NONE) => GREATER *)
                                    { pat = Ptuple [ Papp { id = [mk_id "SOME"]
                                                          , atpat = promote Pwild
                                                          }
                                                   , Pconstr (mk_id "NONE")
                                                   ]
                                    , exp = Eident [mk_id "GREATER"]
                                    }
                                  , (* _ => EQUAL *)
                                    { pat = promote Pwild
                                    , exp = Econstr (mk_id "EQUAL")
                                    }
                                  ]
                              }
                        |> promote
                    | (_, "list") => raise Fail "invalid number of tyargs to list"
                    | (_, "option") => raise Fail "invalid number of tyargs to option"
                    | _ => add_helpers (Eident (long_ty_to_cmp longid))
                    )
                | _ => add_helpers (Eident (long_ty_to_cmp longid))
              )
            end

        (* Functions are incomparable. *)
        | SMLSyntax.Tarrow (ty1, ty2) => raise Fail "trying to compare function types"

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
                    SOME (acc >>= exp)
                  )
                  NONE
                  codegen_tys
                |> (fn NONE => raise Fail "empty product type in cmp"
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
                      SOME (acc >>= exp)
                  )
                  NONE
                  codegen_tys
                |> (fn NONE => raise Fail "empty trecord in cmp"
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

    (* Check if a `deriving` actually has a `cmp`
     *)
    fun verify_deriving deriving =
      case deriving of
        NONE => false
      | SOME plugins =>
          List.find
            (fn (id, _) => id_eq (id, mk_id "cmp") orelse id_eq (id, mk_id "compare"))
            plugins
          |> (fn NONE => false
             | SOME (_, []) => true
             | SOME (_, _::_) => raise Fail "cmp currently does not take options!"
             )

    (* Makes the function declarations for the cmp functions.
     *)
    fun mk_fundec mk_init id =
      let
        val s = id_to_string id
      in
        [ mk_init (mk_id ("cmp_" ^ s))
        , [ { opp = false
            , id = mk_id (s ^ "_cmp")
            , pats = [Pident x]
            , ty = NONE
            , exp = Eident [mk_id ("cmp_" ^ s)] $ Eident [x]
            }
          ]
        , [ { opp = false
            , id = mk_id (s ^ "_compare")
            , pats = [Pident x]
            , ty = NONE
            , exp = Eident [mk_id ("cmp_" ^ s)] $ Eident [x]
            }
          ]
        , [ { opp = false
            , id = mk_id ("compare_" ^ s)
            , pats = [Pident x]
            , ty = NONE
            , exp = Eident [mk_id ("cmp_" ^ s)] $ Eident [x]
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
           * For instance, instead of (x, Int.toString x) for SOME, we need
           * (SOME x, "SOME " ^ Int.toString x)
           *)
          fun add_constructor conid ((pat1, pat2), exp) =
            { pat = Ptuple [ Papp {id = [conid], atpat = pat1}
                           , Papp {id = [conid], atpat = pat2}
                           ]
            , exp = exp
            }

          (* This is a function which takes in a value of the ordered type, and
           * assigns it a number depending on which variant it is.
           *)
          val enum_exp =
            Efn [ { pat = Pident x
                  , exp =
                      Ecase { exp = Eident [x]
                            , matches =
                                List.mapi
                                  (fn (idx, {id = conid, ty, opp}) =>
                                    { pat =
                                      case ty of
                                        NONE => Pident conid
                                      | _ =>
                                        Papp { id = [conid]
                                             , atpat = promote Pwild
                                             }
                                    , exp = Enumber (Int idx) |> promote
                                    }
                                  )
                                  conbinds
                            }
                      |> promote
                  }
                ]

          (* These are the actual cases for the principal argument of the
           * function, which exhaustively handle each constructor.
           *)
          val matches =
            ( List.map
              (fn {id = conid, ty = tyopt, opp} =>
                case tyopt of
                  NONE =>
                    { pat = Ptuple [Pident conid, Pident conid]
                    , exp = Eident [mk_id "EQUAL"]
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
             * This is
             * `(x, y) => Int.compare (enum x, enum y)
             *)
            ( case conbinds of
                [_] => []
               | _ => [ { pat = Ptuple [Pident x, Pident y]
                        , exp = int_compare
                              $ Etuple [ enum_exp $ Eident [x]
                                       , enum_exp $ Eident [y]
                                       ]
                        }
                      ]
            )

          (* The fvalbinds need to be made without going all the way to `Dfun`,
           * because they must be `and`ed together (in the same `Dfun`),
           * to support mutual recursion.
           *)
          fun mk_cmp_fvalbind id =
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
          (* TODO?: can also make it just "cmp" if it's called "t" *)
          mk_fundec mk_cmp_fvalbind tycon
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
                    fun mk_cmp_fvalbind id =
                      [ { opp = false
                        , id = id
                        , pats = fn_pats @ [Ptuple [pat1, pat2]]
                        , ty = NONE
                        , exp = exp
                        }
                      ]
                  in
                    mk_fundec mk_cmp_fvalbind tycon
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
        fun mk_funspec tycon cmp_ty =
          [ SPval { id = mk_id ("cmp_" ^ id_to_string tycon)
                  , ty = cmp_ty
                  }
          , SPval { id = mk_id (id_to_string tycon ^ "_cmp")
                  , ty = cmp_ty
                  }
          , SPval { id = mk_id ("compare_" ^ id_to_string tycon)
                  , ty = cmp_ty
                  }
          , SPval { id = mk_id (id_to_string tycon ^ "_compare")
                  , ty = cmp_ty
                  }
          ]

        fun pair_ty ty = promote (Tprod [ty, ty])
      in
        ( case Node.getVal spec of
          ( SPtype {tyvars, tycon, ty = _, deriving}
          | SPdatdec {tyvars, tycon, condescs = _, deriving} ) =>
            if not (verify_deriving deriving) then
              []
            else
              let
                (* The types of all of the helper functions, for a polymorphic
                 * type.
                 *)
                val helper_tys =
                  List.map
                    (fn tyvar => Tarrow (pair_ty (Ttyvar tyvar), Tident [mk_id "order"]))
                    tyvars

                (* The type we're generating cmp for.
                 *)
                val self_ty =
                  case tyvars of
                    [] => Tident [tycon]
                  | _ => Tapp (List.map Ttyvar tyvars, [tycon])

                val cmp_ty =
                  List.foldr
                    (fn (ty, acc) => Tarrow (ty, acc))
                    (Tarrow (pair_ty self_ty, Tident [mk_id "order"]))
                    helper_tys
              in
                mk_funspec tycon cmp_ty
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
