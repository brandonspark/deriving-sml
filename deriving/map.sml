
structure Map : PLUGIN =
  struct
    open Prelude

    infix |>
    infix $

    fun long_ty_to_map longid =
      case longid of
        [] => raise Fail "impossible, long_ty_to_map on empty id"
      | _ =>
          change_last
            (fn node =>
              Node.map (fn sym => map_sym sym (fn s => s ^ "_map")) node
            )
            longid

    fun all_none l =
      List.foldr
        (fn (NONE, acc) => acc
        | (SOME _, acc) => false
        )
        true
        l

    fun ty_to_code get_tyvar_fn ty ctx (type_name : identifier option) =
      (* type_name should be SOME tycon if we are doing this for a type alias
       * named tycon.
       * otherwise it should be NONE
       *)
      let
        val id = new ()
      in
        case Node.getVal ty of
          SMLSyntax.Tident [ty_id] =>
            (case type_name of
              SOME tycon =>
                if id_eq (ty_id, tycon) then
                  raise Fail "type alias defined in terms of itself"
                else
                  NONE
            | NONE =>
                NONE
            )

        (* Identity for non-polymorphic modular types.
         *)
        | SMLSyntax.Tident longty => NONE

        (* Suppose we're codegen-ing for something like:
         * 'a
         * We don't know how to print an `'a`.
         * But this codegen'd function simply assumes it's given such a
         * polymorphic printing function, referred to as `fni`, for the
         * tyvar's index.
         * (if we were deriving on an ('a, 'b) t, this index would be 0)
         *)
        | SMLSyntax.Ttyvar tyvar =>
            SOME
              ( Pident id
              , get_tyvar_fn tyvar $ Eident [id]
              )

        (* For polymorphic functions, get the right map function and pass it the
         * map functions for the instantiated tyargs.
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
                    (fn (NONE, acc) => acc $ identity
                    | (SOME (pat, exp), acc) =>
                      acc $ Efn [{pat = pat, exp = exp}]
                    )
                    exp
                    codegen_tys
                $ Eident [id]
            in
              if all_none codegen_tys then
                NONE
              else
                SOME
                  ( Pident id
                  , case longid of
                      (* If it's not in a module, it may be `list` or `option`.
                       *)
                      [sing_id] =>
                        ( case (codegen_tys, id_to_string sing_id) of
                          ([SOME (pat, exp)], "list") =>
                              Eident [mk_id "List", mk_id "map"]
                            $ Efn [{pat = pat, exp = exp}]
                            $ Eident [id]
                        | ([SOME (pat, exp)], "option") =>
                              Eident [mk_id "Option", mk_id "map"]
                            $ Efn [{pat = pat, exp = exp}]
                            $ Eident [id]
                        | ([NONE], "list") => raise Fail "impossible"
                        | ([NONE], "option") => raise Fail "impossible"
                        | (_, "list") => raise Fail "invalid number of tyargs to list"
                        | (_, "option") => raise Fail "invalid number of tyargs to option"
                        | _ => add_helpers (Eident (long_ty_to_map longid))
                        )
                    | _ => add_helpers (Eident (long_ty_to_map longid))
                  )
            end

        (* Let's just not map functions. *)
        | SMLSyntax.Tarrow (ty1, ty2) =>
            raise Fail "cannot map for functions"

        (* For product types, codegen the components and combine.
         *)
        | SMLSyntax.Tprod tys =>
            let
              val codegen_tys =
                List.map
                  (fn ty => ty_to_code get_tyvar_fn ty ctx type_name)
                  tys

              fun none_to_id opt =
                case opt of
                  SOME (pat, exp) => (pat, exp)
                | NONE =>
                  let
                    val new_id = new ()
                  in
                    (Pident id, Eident [id])
                  end
            in
              if all_none codegen_tys then
                NONE
              else
                SOME
                  ( Ptuple (List.map (#1 o none_to_id) codegen_tys)
                  , Etuple (List.map (#2 o none_to_id) codegen_tys )
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
            in
              if all_none (List.map #2 codegen_tys) then
                NONE
              else
                let
                  (* If we have some trivial maps on a component of the record,
                   * then we should have it correspond to the identity.
                   *)
                  val codegen_none_to_ident =
                    List.map
                      (fn (lab, SOME (pat, exp)) => (lab, (pat, exp))
                      | (lab, NONE) =>
                          let
                            val new_id = new ()
                          in
                            (lab, (Pident new_id, Eident [new_id]))
                          end
                      )
                      codegen_tys
                in
                  SOME
                    ( promote
                        ( Precord
                            (List.map
                              (fn (lab, (pat, _)) => PRlab {lab = lab, pat = pat})
                              codegen_none_to_ident
                            )
                        )
                    , List.map
                        (fn (lab, (_, exp)) => { lab = lab, exp = exp })
                        codegen_none_to_ident
                      |> Erecord
                      |> promote
                    )
                end
            end
      end

    fun from_ty ty =
      (* TODO: doesn't allow any type variables, but could allow to produce a
       * "partially instantiated" map function
       *)
      let
        val tyvars =
          gather_tyvars ty
          |> msort
              (fn (n1, n2) => Symbol.compare (Node.getVal n1, Node.getVal n2))

        val (fn_pats, get_tyvar_fn) = tyvars_to_fns tyvars
      in
        case
          ( tyvars
          , ty_to_code
              get_tyvar_fn
              ty
              (* TODO: this only works because context is currently unused
               *)
              (Context.init)
              NONE
          )
        of
          ([], _) => raise Fail "deriving map on monomorphic type!"
        | (_, NONE) => ([Pident x], Eident [x])
        | (_, SOME (pat, exp)) => (fn_pats @ [pat], exp)
      end

    (* Check if a `deriving` actually has a `map`
     *)
    fun verify_deriving deriving =
      case deriving of
        NONE => false
      | SOME plugins =>
          List.find
            (fn (id, _) => id_eq (id, mk_id "map"))
            plugins
          |> (fn NONE => false
             | SOME (_, []) => true
             | SOME (_, _::_) => raise Fail "map currently does not take options!"
             )

    (* Makes the function declarations for the map functions.
     *)
    fun mk_fundec mk_init id acc =
      let
        val s = id_to_string id
      in
        [ mk_init (mk_id ("map_" ^ s))
        , [ { opp = false
            , id = mk_id (s ^ "_map")
            , pats = [Pident x]
            , ty = NONE
            , exp = Eident [mk_id ("map_" ^ s)] $ Eident [x]
            }
          ]
        ]
        @ acc
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
            , exp = Eident [conid] $ exp
            }

          (* These are the actual cases for the principal argument of the
           * function, which exhaustively handle each constructor.
           *)
          val matches =
              List.mapPartial
              (fn {id = conid, ty = tyopt, opp} =>
                case tyopt of
                  NONE => NONE
                | SOME ty =>
                    let
                      (* Not a `type` dec, so no need for the name. *)
                      val rec_ans = ty_to_code get_tyvar_fn ty ctx NONE
                    in
                      Option.map (add_constructor conid) rec_ans
                    end
              )
              conbinds

          (* The fvalbinds need to be made without going all the way to `Dfun`,
           * because they must be `and`ed together (in the same `Dfun`),
           * to support mutual recursion.
           *)
          fun mk_map_fvalbind id =
            let
              val principal_arg = new ()
            in
              [ { opp = false
                , id = id
                , pats = fn_pats @ [Pident principal_arg]
                , ty = NONE
                , exp =
                    case matches of
                      [] => Eident [principal_arg]
                    | _ =>
                      Ecase { exp = Eident [principal_arg]
                            , matches = matches
                            }
                      |> promote
                }
              ]
            end
        in
          (* TODO?: can also make it just "map" if it's called "t" *)
          mk_fundec mk_map_fvalbind tycon []
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
                  in
                    case ty_to_code get_tyvar_fn ty ctx (SOME tycon) of
                      NONE => []
                    | SOME (pat, exp) =>
                        let
                          (* The fvalbinds need to be made without going all the way to `Dfun`,
                           * because they must be `and`ed together (in the same `Dfun`),
                           * to support mutual recursion.
                           *)
                          fun mk_map_fvalbind id =
                            [ { opp = false
                              , id = id
                              , pats = fn_pats @ [pat]
                              , ty = NONE
                              , exp = exp
                              }
                            ]
                        in
                          mk_fundec mk_map_fvalbind tycon acc
                        end
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
        fun mk_funspec tycon map_ty =
          [ SPval { id = mk_id ("map_" ^ id_to_string tycon)
                  , ty = map_ty
                  }
          , SPval { id = mk_id (id_to_string tycon ^ "_map")
                  , ty = map_ty
                  }
          ]

      in
        ( case Node.getVal spec of
          ( SPtype {tyvars, tycon, ty = _, deriving}
          | SPdatdec {tyvars, tycon, condescs = _, deriving} ) =>
            if not (verify_deriving deriving) then
              []
            else
              let
                (* Each tyvar in the type maps to a new tyvar. *)
                val tyvar_pairs =
                  List.map
                    (fn tyvar => (tyvar, new ()))
                    tyvars

                fun find_pair tyvar =
                  case
                    List.find (fn (_, tyvar') => id_eq (tyvar, tyvar')) tyvar_pairs
                  of
                    NONE => raise Fail "should be impossible"
                  | SOME (_, x) => x

                (* The type we're generating map for.
                 *)
                val self_ty =
                  case tyvars of
                    [] => Tident [tycon]
                  | _ => Tapp (List.map Ttyvar tyvars, [tycon])

                val helpers_ty =
                  List.foldl
                    (fn (tyvar, acc) =>
                      Tarrow ( acc
                             , Tarrow (Ttyvar tyvar, Ttyvar (find_pair tyvar))
                             )
                    )
                    self_ty
                    tyvars

                local
                  open SMLSyntax
                in
                  (* replace the tyvars by their complement *)
                  fun replace_tyvars ty =
                    Node.map (fn _ =>
                    case Node.getVal ty of
                      Ttyvar tyvar => Ttyvar (find_pair tyvar)
                    | Tapp (tys, longid) =>
                        Tapp (List.map replace_tyvars tys, longid)
                    | Tprod tys => Tprod (List.map replace_tyvars tys)
                    | Tarrow (ty1, ty2) =>
                        Tarrow (replace_tyvars ty1, replace_tyvars ty2)
                    | Trecord fields =>
                        Trecord
                          ( List.map
                            (fn {lab, ty} => {lab = lab, ty = replace_tyvars ty})
                            fields
                          )
                    | Tident x => Tident x
                    ) ty
                end

                val map_ty = (Tarrow (helpers_ty, replace_tyvars self_ty))
              in
                mk_funspec tycon map_ty
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
