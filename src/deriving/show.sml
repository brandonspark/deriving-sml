
structure Show : PLUGIN =
  struct
    open Prelude

    infix $
    infix |>

    fun long_ty_to_show longid =
      case longid of
        [] => raise Fail "impossible, long_ty_to_show on empty id"
      | _ =>
          change_last
            (fn node =>
              Node.map (fn sym => map_sym sym (fn s => s ^ "_show")) node
            )
            longid

    infix ^^
    fun x ^^ y =
      Eapp { left = Eident [mk_id "op^"]
           , right = Etuple [x, y]
           }

    fun ty_to_code get_tyvar_fn ty ctx (type_name : identifier option) =
      (* type_name should be SOME tycon if we are doing this for a type alias
       * named tycon.
       * otherwise it should be NONE
       *)
      let
        val id = new ()

        fun mk_toString id = Eident [mk_id id, mk_id "toString"]

        fun use_show longid id =
            Eident (long_ty_to_show longid)
          $ Eident [id]
      in
        case Node.getVal ty of
          SMLSyntax.Tident [ty_id] =>
            ( Pident id
            , (* Special cases for showing any base types.
               *)
              case id_to_string ty_id of
                "int" =>
                  mk_toString "Int" $ Eident [id]
              | "string" =>
                  (* Since this is producing source code, we have to account for
                   * the backslashes needed to escape in the produced code.
                   * So this is "one level up". This translates to:
                   * \" ^ ... ^ \"
                   *)
                  Estring "\\\"" ^^ Eident [id] ^^ Estring "\\\""
              | "real" =>
                  mk_toString "Real" $ Eident [id]
              | "char" =>
                  Estring "#\\\""
               ^^ (mk_toString "Char" $ Eident [id])
               ^^ Estring "\\\""
              | "bool" =>
                  Eif { exp1 = Eident [id]
                      , exp2 = Estring "true"
                      , exp3 = Estring "false"
                      }
              | "order" =>
                  Ecase
                    { exp = Eident [id]
                    , matches =
                        [ { pat = Pconstr (mk_id "LESS")
                          , exp = Estring "LESS"
                          }
                        , { pat = Pconstr (mk_id "EQUAL")
                          , exp = Estring "EQUAL"
                          }
                        , { pat = Pconstr (mk_id "GREATER")
                          , exp = Estring "GREATER"
                          }
                        ]
                    }
                  |> promote
              | _ =>
                  (* Look for the corresponding show function.
                   *)
                  (case type_name of
                    SOME tycon =>
                      if id_eq (ty_id, tycon) then
                        raise Fail "type alias defined in terms of itself"
                      else
                        use_show [ty_id] id
                  | NONE =>
                      use_show [ty_id] id
                  )
            )

        (* Just use the show function for a modular type.
         *)
        | SMLSyntax.Tident longty => (Pident id, use_show longty id)

        (* Suppose we're codegen-ing for something like:
         * 'a
         * We don't know how to print an `'a`.
         * But this codegen'd function simply assumes it's given such a
         * polymorphic printing function, referred to as `fni`, for the
         * tyvar's index.
         * (if we were deriving on an ('a, 'b) t, this index would be 0)
         *)
        | SMLSyntax.Ttyvar tyvar =>
            ( Pident id
            , get_tyvar_fn tyvar $ Eident [id]
            )

        (* For polymorphic functions, get the right show function and pass it the
         * show functions for the instantiated tyargs.
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
              fun add_printing_fns exp =
                  List.foldl
                    (fn ((pat, exp), acc) =>
                      acc $ Efn [{pat = pat, exp = exp}]
                    )
                    exp
                    codegen_tys
                $ Eident [id]

            in
              ( Pident id
              , case longid of
                  (* If it's not in a module, it may be `list` or `option`.
                   *)
                  [sing_id] =>
                    (case (codegen_tys, id_to_string sing_id) of
                      ([(pat, exp)], "list") =>
                        let
                          val map_fn =
                            Eident [mk_id "List", mk_id "map"] $ Efn [{pat = pat, exp = exp}]
                          val comma_separate =
                            Eident [mk_id "String", mk_id "concatWith"] $ Estring ", "
                        in
                          (* "[" ^ String.concatWith ", " (List.map f id) ^ "]"
                           *)
                             Estring "["
                          ^^ (comma_separate $ (map_fn $ Eident [id]))
                          ^^ Estring "]"
                        end
                    | ([(pat, exp)], "option") =>
                        Ecase { exp = Eident [id]
                              , matches =
                                  [ (* NONE => "NONE" *)
                                    { pat = Pconstr (mk_id "NONE")
                                    , exp = Estring "NONE"
                                    }
                                  , (* SOME x => "SOME " ^ f x *)
                                    { pat = Papp { id = [mk_id "SOME"]
                                                 , atpat = Pident (mk_id "x")
                                                 }
                                    , exp = Estring "SOME ("
                                         ^^ (Efn [{pat = pat, exp = exp}] $ Eident [mk_id "x"])
                                         ^^ Estring ")"
                                    }
                                  ]
                              }
                        |> promote
                    | (_, "list") => raise Fail "invalid number of tyargs to list"
                    | (_, "option") => raise Fail "invalid number of tyargs to option"
                    | _ => add_printing_fns (Eident (long_ty_to_show longid))
                    )
                | _ => add_printing_fns (Eident (long_ty_to_show longid))
              )
            end

        (* Functions are unprintable. *)
        | SMLSyntax.Tarrow (ty1, ty2) =>
            (promote Pwild, Estring "<fn>")

        (* For product types, codegen the components and combine.
         *)
        | SMLSyntax.Tprod tys =>
            let
              val codegen_tys =
                List.map
                  (fn ty => ty_to_code get_tyvar_fn ty ctx type_name)
                  tys
            in
              ( Ptuple (List.map #1 codegen_tys)
              , List.foldl
                  (fn ((_, exp), NONE) => SOME exp
                  | ((_, exp), SOME acc) =>
                    SOME (acc ^^ (Estring ", " ^^ exp))
                  )
                  NONE
                  codegen_tys
                |> (fn NONE => raise Fail "empty product type in show"
                   | SOME exp => Estring "(" ^^ exp ^^ Estring ")"
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
              fun mk_entry lab exp =
                promote (old_estring (Node.getVal lab))
                ^^ Estring " = "
                ^^ exp
            in
              ( promote
                  ( Precord
                      (List.map
                        (fn (lab, (pat, _)) => PRlab {lab = lab, pat = pat})
                        codegen_tys
                      )
                  )
              , List.foldl
                  (fn ((lab, (_, exp)), NONE) =>
                      SOME (mk_entry lab exp)
                  | ((lab, (_, exp)), SOME acc) =>
                      SOME (acc ^^ Estring ", " ^^ mk_entry lab exp)
                  )
                  NONE
                  codegen_tys
                |> (fn NONE => raise Fail "empty trecord in show"
                   | SOME exp => Estring "{" ^^ exp ^^ Estring "}")
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
        |> (fn (x, y) => (fn_pats @ [x], y))
      end

    (* Check if a `deriving` actually has a `show`
     *)
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

    (* Makes the function declarations for the show functions.
     *)
    fun mk_fundec mk_init id =
      let
        val s = id_to_string id
      in
        [ mk_init (mk_id ("show_" ^ s))
        , [ { opp = false
            , id = mk_id (s ^ "_show")
            , pats = [Pident x]
            , ty = NONE
            , exp = Eident [mk_id ("show_" ^ s)] $ Eident [x]
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
          fun add_constructor conid (pat, exp) =
            { pat = Papp { id = [conid]
                         , atpat = pat
                         }
              (* TODO: can do some optimizations here to make less excess parens
               * *)
            , exp = Estring (id_to_string conid ^ " (")
                 ^^ exp
                 ^^ Estring ")"
            }

          (* These are the actual cases for the principal argument of the
           * function, which exhaustively handle each constructor.
           *)
          val matches =
              List.map
              (fn {id = conid, ty = tyopt, opp} =>
                case tyopt of
                  NONE =>
                    { pat = Pident conid, exp = Estring (id_to_string conid) }
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
                , id = id
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
          mk_fundec mk_show_fvalbind tycon
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
                    val (pat, exp) = ty_to_code get_tyvar_fn ty ctx (SOME tycon)

                    (* The fvalbinds need to be made without going all the way to `Dfun`,
                     * because they must be `and`ed together (in the same `Dfun`),
                     * to support mutual recursion.
                     *)
                    fun mk_show_fvalbind id =
                      [ { opp = false
                        , id = id
                        , pats = fn_pats @ [pat]
                        , ty = NONE
                        , exp = exp
                        }
                      ]
                  in
                    mk_fundec mk_show_fvalbind tycon
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
        fun mk_funspec tycon show_ty =
          [ SPval { id = mk_id ("show_" ^ id_to_string tycon)
                  , ty = show_ty
                  }
          , SPval { id = mk_id (id_to_string tycon ^ "_show")
                  , ty = show_ty
                  }
          ]

      in
        ( case Node.getVal spec of
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

                val show_ty = (Tarrow (self_ty, Tident [mk_id "string"]))
              in
                mk_funspec tycon show_ty
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
                mk_funspec tycon show_ty
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
