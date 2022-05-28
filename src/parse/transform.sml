
(* Transform contains all the logic for post-processing the parsed AST.
 *
 * This includes the duties of:
 * - Fixing ambiguities in juxtaposed patterns/expressions due to infix operators
 * - Codegen for derived attributes on types
 *)

signature TRANSFORM =
  sig
    val transform : SMLSyntax.ast -> SMLSyntax.ast
  end

signature PATH =
  sig
    type t

    val add : string -> t -> t
    val cons : t -> (string * t) option
    val concat : t -> t -> t
    val pop : t -> t
    val empty : t

    val to_string : t -> string
    val foldr : (t * 'a) -> 'a -> t -> 'a
  end

structure Transform : TRANSFORM =
  struct
    open SMLSyntax
    open Common
    open Error

    (* Transform is set up such that the canonical form of values is a tuple,
     * where the first projection is whatever part of the AST we are interested
     * in, and the second projection is a context which carries the information
     * we have received up until this point.
     *
     * Think of `||>` as one which "reaches inside" the tuple and only cares
     * about the real element.
     *)
    infix |>
    infix ||>
    fun x ||> f = x |> Pair.map_fst f

    type bindings = unit ScopeDict.t
    datatype assoc = Left | Right

    fun mk_sing (x, ctx) = ([x], ctx)

    (* For when we have the result we need, but we need to map it and place it
     * inside of a separate node. We keep the context, too. *)
    fun expand_node_with_ctx map_f node (result, ctx) =
      (Node.map (fn _ => map_f result) node, ctx)

    (* Simplifies some boilerplate. *)
    fun fold_transform transform_f ctx xs =
      List.foldl
        (fn (x, (xs, ctx)) =>
          transform_f (x, ctx)
          ||> (fn x => x::xs))
        ([], ctx)
        xs
      ||> List.rev

    (****************
     * DECLARATIONS *
     ****************)

    fun transform_strdec (strdec, ctx) =
      let
        open SMLSyntax
      in
        case Node.getVal strdec of
          DMdec dec =>
            transform_dec (dec, ctx)
            |> expand_node_with_ctx DMdec strdec
        | DMstruct mods =>
            fold_transform
              (fn ({id, seal, module}, ctx) =>
                let
                  val (seal, ctx) =
                    case seal of
                      NONE => (NONE, ctx)
                    | SOME { opacity, signat } =>
                        transform_signat (signat, ctx)
                        ||> (fn signat =>
                              SOME
                                { opacity = opacity
                                , signat = signat
                                }
                            )
                in
                  transform_module (module, ctx)
                  ||> (fn module => {id=id, seal=seal, module=module})
                end
              )
              ctx
              mods
            |> expand_node_with_ctx DMstruct strdec
        | DMlocal {left_dec, right_dec} =>
            let
              val new_ctx = Context.new_scope ctx
              val (left_dec, new_ctx) = transform_strdec (left_dec, new_ctx)
              val new_ctx = Context.new_scope new_ctx
              val (right_dec, new_ctx) = transform_strdec (right_dec, new_ctx)
            in
              ({left_dec=left_dec, right_dec=right_dec}, Context.pop_penultimate new_ctx)
              |> expand_node_with_ctx DMlocal strdec
            end
        | DMseq strdecs =>
            fold_transform
              transform_strdec
              ctx
              strdecs
            |> expand_node_with_ctx DMseq strdec
        | DMempty => (strdec, ctx)
      end

    (**********************
     * MODULE EXPRESSIONS *
     **********************)

    and transform_module (module, ctx) =
      let
        open SMLSyntax
      in
        case Node.getVal module of
          Mstruct strdec =>
            let
              val new_ctx = Context.new_scope ctx
              val (strdec, new_ctx) = transform_strdec (strdec, new_ctx)
            in
              (* ctx, because we lose all the information we just got *)
              (strdec, ctx)
              |> expand_node_with_ctx Mstruct module
            end
        | Mseal {module, opacity, signat} =>
            let
              val (module, ctx) = transform_module (module, ctx)
              val (signat, ctx) = transform_signat (signat, ctx)
            in
              ({module=module, opacity=opacity, signat=signat}, ctx)
              |> expand_node_with_ctx Mseal module
            end
        | Mapp {functorr, arg} =>
            ( case arg of
              Normal_app modid =>
                transform_module (modid, ctx)
                ||> (fn module => {functorr=functorr, arg=Normal_app module})
                |> expand_node_with_ctx Mapp module
            | Sugar_app strdec =>
                transform_strdec (strdec, ctx)
                ||> (fn strdec => {functorr=functorr, arg=Sugar_app strdec})
                |> expand_node_with_ctx Mapp module
            )
        | Mlet {dec, module} =>
            (* TODO: this does not seem right *)
            let
              val (strdec, new_ctx) = transform_strdec (dec, ctx)
              val (module, new_ctx) = transform_module (module, new_ctx)
            in
              ({dec=strdec, module=module}, ctx)
              |> expand_node_with_ctx Mlet module
            end
        | Mident _ => (module, ctx)
      end

    and transform_typbinds (typbinds, ctx) =
      fold_transform
        (fn ({tyvars, tycon, ty, deriving}, ctx) =>
          transform_ty (ty, ctx)
          ||> (fn ty => {tyvars=tyvars, tycon=tycon, ty=ty, deriving=deriving}))
        ctx
        typbinds

    and transform_conbinds (conbinds, ctx) =
      fold_transform
        (fn ({opp, id, ty}, ctx) =>
          case ty of
            NONE => ({opp=opp, id=id, ty=NONE}, ctx)
          | SOME ty =>
              transform_ty (ty, ctx)
              ||> (fn ty => {opp=opp, id=id, ty=SOME ty})
        )
        ctx
        conbinds

    and transform_datbinds (datbinds, ctx) =
      fold_transform
        (fn ({tyvars, tycon, conbinds, deriving}, ctx) =>
          transform_conbinds (conbinds, ctx)
          ||> (fn conbinds => {tyvars=tyvars, tycon=tycon, conbinds=conbinds, deriving=deriving})
        )
        ctx
        datbinds

    and transform_fvalbind (fvalbind, ctx) =
      fold_transform
        (fn ({opp, id, pats, ty, exp}, ctx) =>
          let
            val (pats, ctx) =
              fold_transform
                transform_pat
                ctx
                pats
            val (ty, ctx) =
              case ty of
                NONE => (NONE, ctx)
              | SOME ty => transform_ty (ty, ctx) ||> SOME
            val (exp, ctx) = transform_exp (exp, ctx)
          in
            ({opp=opp, id=id, pats=pats, ty=ty, exp=exp}, ctx)
          end)
        ctx
        fvalbind

    and transform_fvalbinds (fvalbinds, ctx) =
      fold_transform
        transform_fvalbind
        ctx
        fvalbinds

    and transform_exbinds (exbinds, ctx) = (exbinds, ctx)

    (****************
     * DECLARATIONS *
     ****************)

    (* transform_dec' turns one declaration into one or more declarations
     *)
    and transform_dec' (dec, ctx) =
      let
        open SMLSyntax
      in
        case Node.getVal dec of
          Dseq decs =>
            fold_transform
              transform_dec
              ctx
              decs
            ||> ( List.foldr
                  (fn (dec, acc) =>
                    (case Node.getVal dec of
                      Dseq decs => decs @ acc
                    | _ => dec :: acc
                    )
                  )
                  []
                )
        | _ =>
            (case Node.getVal dec of
              Dseq _ => raise Fail "literally impossible"
            | Dval {recc, tyvars, valbinds} =>
                fold_transform
                  (fn ({pat, exp}, ctx) =>
                    let
                      val (pat, ctx) = transform_pat (pat, ctx)
                      val (exp, ctx) = transform_exp (exp, ctx)
                    in
                      ({pat=pat, exp=exp}, ctx)
                    end)
                  ctx
                  valbinds
                ||> (fn valbinds => {recc=recc, tyvars=tyvars, valbinds=valbinds})
                |> expand_node_with_ctx Dval dec
            | Dfun {tyvars, fvalbinds} =>
                let
                  val (fvalbinds, ctx) = transform_fvalbinds (fvalbinds, ctx)
                in
                  ({tyvars=tyvars, fvalbinds=fvalbinds}, ctx)
                  |> expand_node_with_ctx Dfun dec
                end
            | Dtype typbinds =>
                transform_typbinds (typbinds, ctx)
                |> expand_node_with_ctx Dtype dec

            (* DERIVING STUFF HERE
             * So, because we have to do code generation on the AST, we have to
             * start at a higher granularity than just the `datbind` itself. So, we
             * change any instance of a `Ddatdec`*)
            | Ddatdec {datbinds, withtypee} =>
                let
                  val (datbinds, ctx) = transform_datbinds (datbinds, ctx)
                  val (withtypee, ctx) =
                    case withtypee of
                      SOME typbinds =>
                        let val (typbinds, ctx) = transform_typbinds (typbinds, ctx) in
                          (SOME typbinds, ctx)
                        end
                    | NONE =>
                        (NONE, ctx)
                in
                  ({datbinds=datbinds, withtypee=withtypee}, ctx)
                  |> expand_node_with_ctx Ddatdec dec
                end

            | Dabstype {datbinds, withtypee, withh} =>
                let
                  val (datbinds, ctx) = transform_datbinds (datbinds, ctx)
                  val (withtypee, ctx) =
                    case withtypee of
                      SOME typbinds =>
                        let
                          val (typbinds, ctx) = transform_typbinds (typbinds, ctx)
                        in
                          (SOME typbinds, ctx)
                        end
                    | NONE => (NONE, ctx)
                  val (withh, ctx) = transform_dec (withh, ctx)
                in
                  ({datbinds=datbinds, withtypee=withtypee, withh=withh}, ctx)
                  |> expand_node_with_ctx Dabstype dec
                end

            | Dexception exbinds =>
                transform_exbinds (exbinds, ctx)
                |> expand_node_with_ctx Dexception dec

            | Dlocal {left_dec, right_dec} =>
                let
                  (* For the stuff declared in the `local` *)
                  val new_ctx = Context.new_scope ctx
                  val (left_dec, new_ctx) = transform_dec (left_dec, new_ctx)
                  (* For the stuff declared in the `in` *)
                  val new_ctx = Context.new_scope new_ctx
                  val (right_dec, new_ctx) = transform_dec (right_dec, new_ctx)
                in
                  (* Pop penultimate, to get rid of the local stuff *)
                  ( {left_dec=left_dec, right_dec=right_dec}
                  , Context.pop_penultimate new_ctx)
                  |> expand_node_with_ctx Dlocal dec
                end

            | Dinfix {precedence, ids} =>
                let
                  val precedence = Option.getOpt (precedence, 0)
                  val new_ctx =
                    List.foldl
                      (fn (id, ctx) =>
                        Context.add_infix
                          (SMLSyntax.id_to_string id)
                          {assoc=Context.Left, precedence=precedence}
                          ctx)
                      ctx
                      ids
                in
                  (dec, new_ctx)
                end

            | Dinfixr {precedence, ids} =>
              let
                val precedence = Option.getOpt (precedence, 0)
                val new_ctx =
                  List.foldl
                    (fn (id, ctx) =>
                      Context.add_infix
                        (SMLSyntax.id_to_string id)
                        {assoc=Context.Right, precedence=precedence}
                        ctx)
                    ctx
                    ids
              in
                (dec, new_ctx)
              end

            | Dnonfix ids =>
              let
                val new_ctx =
                  List.foldl
                    (fn (id, ctx) => Context.remove_infix (SMLSyntax.id_to_string id) ctx)
                    ctx
                    ids
              in
                (dec, new_ctx)
              end

            | ( Ddatrepl _ (* TODO: datrepl deriving *)
              | Dopen _
              | Dempty ) =>
                  (dec, ctx)
            )
            |> Derive.codegen_dec
      end
    and transform_dec (dec, ctx) =
      case transform_dec' (dec, ctx) of
        ([], _) => raise Fail "I think something went wrong"
      | ([dec], ctx) => (dec, ctx)
      | (mult, ctx) => (Node.create_absurd (Dseq mult), ctx)

    (***************
     * EXPRESSIONS *
     ***************)

    and transform_exp (exp, ctx) =
      let
        open SMLSyntax
      in
        case Node.getVal exp of
          Erecord exprows =>
            fold_transform
              (fn ({lab,exp}, ctx) =>
                transform_exp (exp, ctx)
                ||> (fn exp => {lab=lab, exp=exp}))
              ctx
              exprows
            |> expand_node_with_ctx Erecord exp
        | Etuple exps =>
            fold_transform
              transform_exp
              ctx
              exps
            |> expand_node_with_ctx Etuple exp
        | Elist exps =>
            fold_transform
              transform_exp
              ctx
              exps
            |> expand_node_with_ctx Elist exp
        | Eseq exps =>
            fold_transform
              transform_exp
              ctx
              exps
            |> expand_node_with_ctx Eseq exp
        | Elet {dec, exps} =>
            let
              val (dec, ctx) = transform_dec (dec, ctx)
            in
              fold_transform
                transform_exp
                ctx
                exps
              ||> (fn exps => {dec=dec, exps=exps})
              |> expand_node_with_ctx Elet exp
            end
        | Eapp {left, right} =>
            let
              val (left, ctx) = transform_exp (left, ctx)
              val (right, ctx) = transform_exp (right, ctx)
            in
              ({left=left, right=right}, ctx)
              |> expand_node_with_ctx Eapp exp
            end
        | Etyped {exp, ty} =>
            let
              val (exp, ctx) = transform_exp (exp, ctx)
              val (ty, ctx) = transform_ty (ty, ctx)
            in
              ({exp=exp, ty=ty}, ctx)
              |> expand_node_with_ctx Etyped exp
            end
        | Eandalso {left, right} =>
            let
              val (left, ctx) = transform_exp (left, ctx)
              val (right, ctx) = transform_exp (right, ctx)
            in
              ({left=left, right=right}, ctx)
              |> expand_node_with_ctx Eandalso exp
            end
        | Eorelse {left, right} =>
            let
              val (left, ctx) = transform_exp (left, ctx)
              val (right, ctx) = transform_exp (right, ctx)
            in
              ({left=left, right=right}, ctx)
              |> expand_node_with_ctx Eorelse exp
            end
        | Ehandle {exp, matches} =>
            let
              val (exp, ctx) = transform_exp (exp, ctx)
            in
              fold_transform
                (fn ({pat, exp}, ctx) =>
                  let
                    val (pat, ctx) = transform_pat (pat, ctx)
                    val (exp, ctx) = transform_exp (exp, ctx)
                  in
                    ({pat=pat, exp=exp}, ctx)
                  end)
                ctx
                matches
              ||> (fn matches => {exp=exp, matches=matches})
              |> expand_node_with_ctx Ehandle exp
            end
        | Eraise exp =>
            transform_exp (exp, ctx)
            |> expand_node_with_ctx Eraise exp
        | Eif {exp1, exp2, exp3} =>
            let
              val (exp1, ctx) = transform_exp (exp1, ctx)
              val (exp2, ctx) = transform_exp (exp2, ctx)
              val (exp3, ctx) = transform_exp (exp3, ctx)
            in
              ({exp1=exp1, exp2=exp2, exp3=exp3}, ctx)
              |> expand_node_with_ctx Eif exp
            end
        | Ewhile {exp1, exp2} =>
            let
              val (exp1, ctx) = transform_exp (exp1, ctx)
              val (exp2, ctx) = transform_exp (exp2, ctx)
            in
              ({exp1=exp1, exp2=exp2}, ctx)
              |> expand_node_with_ctx Ewhile exp
            end
        | Ecase {exp=exp', matches} =>
            let
              val (exp', ctx) = transform_exp (exp', ctx)
            in
              fold_transform
                (fn ({pat, exp}, ctx) =>
                  let
                    val (pat, ctx) = transform_pat (pat, ctx)
                    val (exp, ctx) = transform_exp (exp, ctx)
                  in
                    ({pat=pat, exp=exp}, ctx)
                  end)
                ctx
                matches
              ||> (fn matches => {exp=exp', matches=matches})
              |> expand_node_with_ctx Ecase exp
            end
        | Efn matches =>
            fold_transform
              (fn ({pat, exp}, ctx) =>
                let
                  val (pat, ctx) = transform_pat (pat, ctx)
                  val (exp, ctx) = transform_exp (exp, ctx)
                in
                  ({pat=pat, exp=exp}, ctx)
                end)
              ctx
              matches
            |> expand_node_with_ctx Efn exp
        | Ejuxta ejuxtas =>
            let
              fun transform_ejuxta (ejuxta, ctx) =
                case ejuxta of
                  Jident (id, exp) => (Jident (id, exp), ctx)
                | Jatom exp =>
                    (case Node.getVal exp of
                      Eident _ => (Jatom exp, ctx)
                    | Econstr _ => (Jatom exp, ctx)
                    | _ =>
                        transform_exp (exp, ctx)
                        ||> (fn exp => Jatom exp))

              val (ejuxtas, ctx) =
                fold_transform
                  transform_ejuxta
                  ctx
                  ejuxtas
            in
              SMLSyntax.juxta_span Node.list_span ejuxtas
              |> ExpPrecedence.parse ctx ejuxtas
              |> (fn result => (result, ctx))
            end
        | Eident {opp=false, id} =>
            (case Context.lookup_infix (longid_to_string id) ctx of
              SOME _ =>
                err
                  (FixityError
                    { reason = "lone infix identifier without `op`"
                    , span = Node.getSpan (ListUtils.last id)
                    }
                  )
            | _ => (exp, ctx))
        | Econstr {opp=false, id} =>
            (case Context.lookup_infix (longid_to_string id) ctx of
              SOME _ =>
                err
                  (FixityError
                    { reason = "lone infix constructor without `op`"
                    , span = Node.getSpan (ListUtils.last id)
                    }
                  )
            | _ => (exp, ctx))
        | ( Enumber _
          | Estring _
          | Echar _
          | Eselect _
          | Eunit
          | Eident _
          | Econstr _ ) => (exp, ctx)
      end

    (****************
     * ROW PATTERNS *
     ****************)

    and transform_patrow (patrow, ctx) =
      let
        open SMLSyntax
      in
        case patrow of
          PRlab {lab, pat} =>
            transform_pat (pat, ctx)
            ||> (fn pat => {lab=lab, pat=pat})
            ||> PRlab
        | PRas {id, ty, aspat} =>
            let
              val (ty, ctx) =
                case ty of
                  SOME ty => transform_ty (ty, ctx) ||> SOME
                | NONE => (NONE, ctx)
              val (aspat, ctx) =
                case aspat of
                  SOME aspat => transform_pat (aspat, ctx) ||> SOME
                | NONE => (NONE, ctx)
            in
              (PRas {id=id, ty=ty, aspat=aspat}, ctx)
            end
        | PRellipsis => (patrow, ctx)
      end

    (************
     * PATTERNS *
     ************)

    and transform_pat (pat, ctx) =
      let
        open SMLSyntax
      in
        case Node.getVal pat of
          Precord patrows =>
            fold_transform
              transform_patrow
              ctx
              patrows
            |> expand_node_with_ctx Precord pat
        | Ptuple pats =>
            fold_transform
              transform_pat
              ctx
              pats
            |> expand_node_with_ctx Ptuple pat
        | Plist pats =>
            fold_transform
              transform_pat
              ctx
              pats
            |> expand_node_with_ctx Plist pat
        | Por pats =>
            fold_transform
              transform_pat
              ctx
              pats
            |> expand_node_with_ctx Por pat
        | Papp {id, atpat} =>
            transform_pat (atpat, ctx)
            ||> (fn atpat => {id=id, atpat=atpat})
            |> expand_node_with_ctx Papp pat
        | Ptyped {pat, ty} =>
            let
              val (pat, ctx) = transform_pat (pat, ctx)
              val (ty, ctx) = transform_ty (ty, ctx)
            in
              ({pat=pat, ty=ty}, ctx)
              |> expand_node_with_ctx Ptyped pat
            end
        | Playered {opp, id, ty, aspat} =>
            let
              val (ty, ctx) =
                case ty of
                  SOME ty => transform_ty (ty, ctx) ||> SOME
                | NONE => (ty, ctx)
              val (aspat, ctx) = transform_pat (aspat, ctx)
            in
              ({opp=opp, id=id, ty=ty, aspat=aspat}, ctx)
              |> expand_node_with_ctx Playered pat
            end
        | Pjuxta pjuxtas =>
            let
              fun transform_pjuxta (pjuxta, ctx) =
                case pjuxta of
                  Jident (id, pat) => (Jident (id, pat), ctx)
                | Jatom pat =>
                    (case Node.getVal pat of
                      Pident _ => (Jatom pat, ctx)
                    | Pconstr _ => (Jatom pat, ctx)
                    | _ =>
                        transform_pat (pat, ctx)
                        ||> (fn pat => Jatom pat))

              val (pjuxtas, ctx) =
                fold_transform
                  transform_pjuxta
                  ctx
                  pjuxtas
            in
              SMLSyntax.juxta_span Node.list_span pjuxtas
              |> PatPrecedence.parse ctx pjuxtas
              |> (fn result => (result, ctx))
            end
        | Pident {opp=false, id} =>
            (case Context.lookup_infix (id_to_string id) ctx of
              (* NOTE: An infix Pident is being applied prefix.
               * For now, allow it.
               *)
              SOME _ => (pat, ctx)
            | _ => (pat, ctx))
        | Pconstr {opp=false, id} =>
            (case Context.lookup_infix (longid_to_string id) ctx of
              (* NOTE: An infix Pconstr is being applied prefix.
               * For now, allow it.
               *)
              SOME _ => (pat, ctx)
            | _ => (pat, ctx))
        | ( Pnumber _
          | Pword _
          | Pstring _
          | Pchar _
          | Pwild
          | Pident _
          | Pconstr _
          | Punit ) => (pat, ctx)
      end

    and transform_ty (ty, ctx) = (ty, ctx)

    and transform_fundec (fundec, ctx) =
      let
        open SMLSyntax
        val funbinds = Node.getVal fundec
      in
        fold_transform
          (fn (funbind_node, ctx) =>
            let
              val {id, funarg, seal, body} = Node.getVal funbind_node
              val (funarg, ctx) =
                case funarg of
                  Normal {id, signat} =>
                    transform_signat (signat, ctx)
                    ||> (fn signat => Normal {id = id, signat = signat})
                | Sugar specs =>
                    List.foldl
                      (fn (spec, (specs, ctx)) =>
                        transform_spec (spec, ctx)
                        ||> (fn new_specs => specs @ new_specs))
                      ([], ctx)
                      specs
                    ||> (fn specs => Sugar specs)
              val (seal, ctx) =
                case seal of
                  NONE => (NONE, ctx)
                | SOME {signat, opacity} =>
                    transform_signat (signat, ctx)
                    ||> (fn signat => SOME {signat = signat, opacity = opacity})
              val (body, ctx) = transform_module (body, ctx)
            in
              (Node.map
                (Fn.const {id=id, funarg = funarg, seal=seal, body=body})
                funbind_node, ctx)
            end)
          ctx
          funbinds
        |> expand_node_with_ctx Fn.id fundec
      end

    and transform_sigdec (sigdec, ctx) =
      let
        open SMLSyntax
        val sigbinds = Node.getVal sigdec
      in
        fold_transform
          (fn ({id, signat}, ctx) =>
            let
              val (signat, ctx) = transform_signat (signat, ctx)
            in
              ({id=id, signat=signat}, ctx)
            end)
          ctx
          sigbinds
        |> expand_node_with_ctx Fn.id sigdec
      end

    (*************************
     * SIGNATURE EXPRESSIONS *
     *************************)

    and transform_signat (signat, ctx) =
      let
        open SMLSyntax
      in
        case Node.getVal signat of
          Sspec specs =>
            List.foldl
              (fn (spec, (specs, ctx)) =>
                transform_spec (spec, ctx)
                ||> (fn new_specs => specs @ new_specs))
              ([], ctx)
              specs
            |> expand_node_with_ctx Sspec signat
        | Sident ident => (signat, ctx)
        | Swhere {signat, wheretypee={tyvars, id, ty}} =>
            let
              val (signat, ctx) = transform_signat (signat, ctx)
              val (ty, ctx) = transform_ty (ty, ctx)
            in
              ({signat=signat, wheretypee={tyvars=tyvars, id=id, ty=ty}}, ctx)
              |> expand_node_with_ctx Swhere signat
            end
      end

    and transform_condescs (condescs, ctx) =
      fold_transform
        (fn ({id, ty}, ctx) =>
          case ty of
            NONE => ({id=id, ty=NONE}, ctx)
          | SOME ty =>
              transform_ty (ty, ctx)
              ||> (fn ty => {id=id, ty=SOME ty}))
        ctx
        condescs

    and transform_typdesc ({tyvars, tycon, ty, deriving}, ctx) =
      case ty of
        NONE => ({tyvars=tyvars, tycon=tycon, ty=NONE, deriving=deriving}, ctx)
      | SOME ty =>
          transform_ty (ty, ctx)
          ||> (fn ty => {tyvars=tyvars, tycon=tycon, ty=SOME ty, deriving=deriving})

    (****************************
     * SIGNATURE SPECIFICATIONS *
     ****************************)

    (* transform_spec turns a spec into one or more specs *)
    and transform_spec (spec, ctx) =
      let open SMLSyntax in
        ( case Node.getVal spec of
            SPval {id, ty} =>
              transform_ty (ty, ctx)
              ||> (fn ty => {id=id, ty=ty})
              |> expand_node_with_ctx SPval spec
          | SPtype typdesc =>
              transform_typdesc (typdesc, ctx)
              |> expand_node_with_ctx SPtype spec
          | SPeqtype typdesc =>
              transform_typdesc (typdesc, ctx)
              |> expand_node_with_ctx SPeqtype spec
          | SPdatdec {tyvars, tycon, condescs, deriving} =>
              transform_condescs (condescs, ctx)
              ||> (fn condescs =>
                  {tyvars=tyvars, tycon=tycon, condescs=condescs, deriving=deriving})
              |> expand_node_with_ctx SPdatdec spec
          | SPexception {id, ty} =>
              let
                val (ty, ctx) =
                  case ty of
                    NONE => (NONE, ctx)
                  | SOME ty => transform_ty (ty, ctx) ||> SOME
              in
                ({id=id, ty=ty}, ctx)
              end
              |> expand_node_with_ctx SPexception spec
          | SPmodule {id, signat} =>
              transform_signat (signat, ctx)
              ||> (fn signat => {id=id, signat=signat})
              |> expand_node_with_ctx SPmodule spec
          | SPinclude signat =>
              transform_signat (signat, ctx)
              |> expand_node_with_ctx SPinclude spec
          | SPsharing {specs, tycons} =>
              List.foldl
                (fn (spec, (specs, ctx)) =>
                  transform_spec (spec, ctx)
                  ||> (fn new_specs => specs @ new_specs))
                ([], ctx)
                specs
              ||> (fn specs => {specs=specs, tycons=tycons})
              |> expand_node_with_ctx SPsharing spec
          | ( SPdatrepl _ ) =>
              (spec, ctx)
        )
        |> Derive.codegen_spec
      end

    (* when entering a module, push a new scope
     * when exiting the module, take everything in the new scope and append a
     * `Module.` to it *)
    fun transform topdecs =
      let
        open SMLSyntax
        fun transform_topdec (topdec, ctx) =
          case topdec of
            Strdec strdec =>
              transform_strdec (strdec, ctx)
              ||> Strdec
          | Sigdec sigdec =>
              transform_sigdec (sigdec, ctx)
              ||> Sigdec
          | Fundec fundec =>
              transform_fundec (fundec, ctx)
              ||> Fundec
        val (new_topdecs, _) =
          fold_transform
            transform_topdec
            Context.init
            topdecs
      in
        new_topdecs
      end
  end
