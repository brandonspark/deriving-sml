
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
    infix |>

    type bindings = unit ScopeDict.t
    datatype assoc = Left | Right

    (* For when we have the result we need, but we need to map it and place it
     * inside of a separate node. We keep the context, too. *)
    fun expand_node_with_ctx map_f node (result, ctx) =
      (Node.map (fn _ => map_f result) node, ctx)

    (* Simplifies some boilerplate. *)
    fun fold_transform transform_f ctx xs =
      List.foldl
        (fn (x, (xs, ctx)) =>
          transform_f ctx x
          |> Pair.map_fst (fn x => x::xs))
        ([], ctx)
        xs
      |> Pair.map_fst List.rev

    fun transform_strdec ctx strdec =
      let open SMLSyntax in
        case Node.getVal strdec of
          DMdec dec =>
            transform_dec ctx dec
            |> expand_node_with_ctx DMdec strdec
        | DMstruct mods =>
            List.foldl
              (fn ({id, seal, module}, (mods, ctx)) =>
                transform_module ctx module
                |> Pair.map_fst
                    (fn module => {id=id, seal=seal, module=module}::mods)
              )
              ([], ctx)
              mods
            |> Pair.map_fst List.rev
            |> expand_node_with_ctx DMstruct strdec
        | DMlocal {left_dec, right_dec} =>
            let
              val new_ctx = Context.new_scope ctx
              val (left_dec, new_ctx) = transform_strdec new_ctx left_dec
              val new_ctx = Context.new_scope new_ctx
              val (right_dec, new_ctx) = transform_strdec new_ctx right_dec
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

    and transform_module ctx module =
      let open SMLSyntax in
        case Node.getVal module of
          Mstruct strdec =>
            let
              val new_ctx = Context.new_scope ctx
              val (strdec, new_ctx) = transform_strdec new_ctx strdec
            in
              (strdec, ctx)
              |> expand_node_with_ctx Mstruct module
            end
        | Mseal {module, opacity, signat} =>
            let
              val (module, ctx) = transform_module ctx module
              val (signat, ctx) = transform_signat ctx signat
            in
              ({module=module, opacity=opacity, signat=signat}, ctx)
              |> expand_node_with_ctx Mseal module
            end
        | Mapp {functorr, module} =>
            transform_module ctx module
            |> Pair.map_fst (fn module => {functorr=functorr, module=module})
            |> expand_node_with_ctx Mapp module
        | Mlet {dec, module} =>
            let
              val (strdec, new_ctx) = transform_strdec ctx dec
              val (module, new_ctx) = transform_module new_ctx module
            in
              ({dec=strdec, module=module}, ctx)
              |> expand_node_with_ctx Mlet module
            end
        | Mident _ => (module, ctx)
      end

    and transform_typbinds ctx typbinds =
      List.foldl
        (fn ({tyvars, tycon, ty}, (typbinds, ctx)) =>
          transform_ty ctx ty
          |> Pair.map_fst (fn ty => {tyvars=tyvars, tycon=tycon, ty=ty}::typbinds))
        ([], ctx)
        typbinds
      |> Pair.map_fst List.rev

    and transform_conbinds ctx conbinds =
      List.foldl
        (fn ({opp, id, ty}, (conbinds, ctx)) =>
          case ty of
            NONE => ({opp=opp, id=id, ty=NONE}::conbinds, ctx)
          | SOME ty =>
              transform_ty ctx ty
              |> Pair.map_fst (fn ty => {opp=opp, id=id, ty=SOME ty})
              |> Pair.map_fst (fn result => result::conbinds))
        ([], ctx)
        conbinds
      |> Pair.map_fst List.rev

    and transform_datbinds ctx datbinds =
      List.foldl
        (fn ({tyvars, tycon, conbinds, deriving}, (datbinds, ctx)) =>
          transform_conbinds ctx conbinds
          |> Pair.map_fst (fn conbinds =>
              {tyvars=tyvars, tycon=tycon, conbinds=conbinds, deriving=deriving})
          |> Pair.map_fst (fn result => result::datbinds))
        ([], ctx)
        datbinds
      |> Pair.map_fst List.rev

    and transform_fvalbind ctx fvalbind =
      List.foldl
        (fn ({opp, id, pats, ty, exp}, (fvalbind, ctx)) =>
          let
            val (pats, ctx) =
              fold_transform
                transform_pat
                ctx
                pats
            val (ty, ctx) =
              case ty of
                NONE => (NONE, ctx)
              | SOME ty => transform_ty ctx ty |> Pair.map_fst SOME
            val (exp, ctx) = transform_exp ctx exp
          in
            ({opp=opp, id=id, pats=pats, ty=ty, exp=exp}::fvalbind, ctx)
          end)
        ([], ctx)
        fvalbind
      |> Pair.map_fst List.rev

    and transform_fvalbinds ctx fvalbinds =
      fold_transform
        transform_fvalbind
        ctx
        fvalbinds

    and transform_exbinds ctx exbinds = (exbinds, ctx)

    and transform_dec ctx dec =
      let open SMLSyntax in
        case Node.getVal dec of
          Dval {recc, tyvars, valbinds} =>
            List.foldl
              (fn ({pat, exp}, (valbinds, ctx)) =>
                let
                  val (pat, ctx) = transform_pat ctx pat
                  val (exp, ctx) = transform_exp ctx exp
                in
                  ({pat=pat, exp=exp}::valbinds, ctx)
                end)
              ([], ctx)
              valbinds
            |> Pair.map_fst (fn valbinds => {recc=recc, tyvars=tyvars, valbinds=List.rev valbinds})
            |> expand_node_with_ctx Dval dec
        | Dfun {tyvars, fvalbinds} =>
            let
              val (fvalbinds, ctx) = transform_fvalbinds ctx fvalbinds
            in
              ({tyvars=tyvars, fvalbinds=fvalbinds}, ctx)
              |> expand_node_with_ctx Dfun dec
            end
        | Dtype typbinds =>
            transform_typbinds ctx typbinds
            |> expand_node_with_ctx Dtype dec
        | Ddatdec {datbinds, withtypee} =>
            let
              val (datbinds, ctx) = transform_datbinds ctx datbinds
              val (withtypee, ctx) =
                case withtypee of
                  SOME typbinds =>
                    let val (typbinds, ctx) = transform_typbinds ctx typbinds in
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
              val (datbinds, ctx) = transform_datbinds ctx datbinds
              val (withtypee, ctx) =
                case withtypee of
                  SOME typbinds =>
                    let val (typbinds, ctx) = transform_typbinds ctx typbinds in
                      (SOME typbinds, ctx)
                    end
                | NONE => (NONE, ctx)
              val (withh, ctx) = transform_dec ctx dec
            in
              ({datbinds=datbinds, withtypee=withtypee, withh=withh}, ctx)
              |> expand_node_with_ctx Dabstype dec
            end
        | Dexception exbinds =>
            transform_exbinds ctx exbinds
            |> expand_node_with_ctx Dexception dec
        | Dlocal {left_dec, right_dec} =>
            let
              val new_ctx = Context.new_scope ctx
              val (left_dec, new_ctx) = transform_dec new_ctx left_dec
              val new_ctx = Context.new_scope new_ctx
              val (right_dec, new_ctx) = transform_dec new_ctx right_dec
            in
              ({left_dec=left_dec, right_dec=right_dec}, Context.pop_penultimate new_ctx)
              |> expand_node_with_ctx Dlocal dec
            end
        | Dseq decs =>
            fold_transform
              transform_dec
              ctx
              decs
            |> expand_node_with_ctx Dseq dec
        | Dinfix {precedence, ids} =>
            let
              val precedence = OptionMonad.value precedence 0
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
              val precedence = OptionMonad.value precedence 0
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
        | ( Ddatrepl _
          | Dopen _
          | Dempty ) => (dec, ctx)
      end

    and transform_exp ctx exp =
      let open SMLSyntax in
        case Node.getVal exp of
          Erecord exprows =>
            List.foldl
              (fn ({lab,exp}, (exprows, ctx)) =>
                transform_exp ctx exp
                |> Pair.map_fst (fn exp => {lab=lab, exp=exp}::exprows))
              ([], ctx)
              exprows
            |> Pair.map_fst List.rev
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
              val (dec, ctx) = transform_dec ctx dec
            in
              fold_transform
                transform_exp
                ctx
                exps
              |> Pair.map_fst (fn exps => {dec=dec, exps=exps})
              |> expand_node_with_ctx Elet exp
            end
        | Eapp {left, right} =>
            let
              val (left, ctx) = transform_exp ctx left
              val (right, ctx) = transform_exp ctx right
            in
              ({left=left, right=right}, ctx)
              |> expand_node_with_ctx Eapp exp
            end
        | Etyped {exp, ty} =>
            let
              val (exp, ctx) = transform_exp ctx exp
              val (ty, ctx) = transform_ty ctx ty
            in
              ({exp=exp, ty=ty}, ctx)
              |> expand_node_with_ctx Etyped exp
            end
        | Eandalso {left, right} =>
            let
              val (left, ctx) = transform_exp ctx left
              val (right, ctx) = transform_exp ctx right
            in
              ({left=left, right=right}, ctx)
              |> expand_node_with_ctx Eandalso exp
            end
        | Eorelse {left, right} =>
            let
              val (left, ctx) = transform_exp ctx left
              val (right, ctx) = transform_exp ctx right
            in
              ({left=left, right=right}, ctx)
              |> expand_node_with_ctx Eorelse exp
            end
        | Ehandle {exp, matches} =>
            let
              val (exp, ctx) = transform_exp ctx exp
            in
              List.foldl
                (fn ({pat, exp}, (matches, ctx)) =>
                  let
                    val (pat, ctx) = transform_pat ctx pat
                    val (exp, ctx) = transform_exp ctx exp
                  in
                    ({pat=pat, exp=exp}::matches, ctx)
                  end)
                ([], ctx)
                matches
              |> Pair.map_fst (fn matches => {exp=exp, matches=List.rev matches})
              |> expand_node_with_ctx Ehandle exp
            end
        | Eraise exp =>
            transform_exp ctx exp
            |> expand_node_with_ctx Eraise exp
        | Eif {exp1, exp2, exp3} =>
            let
              val (exp1, ctx) = transform_exp ctx exp1
              val (exp2, ctx) = transform_exp ctx exp2
              val (exp2, ctx) = transform_exp ctx exp3
            in
              ({exp1=exp1, exp2=exp2, exp3=exp3}, ctx)
              |> expand_node_with_ctx Eif exp
            end
        | Ewhile {exp1, exp2} =>
            let
              val (exp1, ctx) = transform_exp ctx exp1
              val (exp2, ctx) = transform_exp ctx exp2
            in
              ({exp1=exp1, exp2=exp2}, ctx)
              |> expand_node_with_ctx Ewhile exp
            end
        | Ecase {exp=exp', matches} =>
            let
              val (exp', ctx) = transform_exp ctx exp'
            in
              List.foldl
                (fn ({pat, exp}, (matches, ctx)) =>
                  let
                    val (pat, ctx) = transform_pat ctx pat
                    val (exp, ctx) = transform_exp ctx exp
                  in
                    ({pat=pat, exp=exp}::matches, ctx)
                  end)
                ([], ctx)
                matches
              |> Pair.map_fst List.rev
              |> Pair.map_fst (fn matches => {exp=exp', matches=matches})
              |> expand_node_with_ctx Ecase exp
            end
        | Efn matches =>
            List.foldl
              (fn ({pat, exp}, (matches, ctx)) =>
                let
                  val (pat, ctx) = transform_pat ctx pat
                  val (exp, ctx) = transform_exp ctx exp
                in
                  ({pat=pat, exp=exp}::matches, ctx)
                end)
              ([], ctx)
              matches
            |> Pair.map_fst List.rev
            |> expand_node_with_ctx Efn exp
        | Ejuxta ejuxtas =>
            let
              fun transform_ejuxta ctx ejuxta =
                case ejuxta of
                  Jident (id, exp) => (Jident (id, exp), ctx)
                | Jatom exp =>
                    (case Node.getVal exp of
                      Eident _ => (Jatom exp, ctx)
                    | Econstr _ => (Jatom exp, ctx)
                    | _ =>
                        transform_exp ctx exp
                        |> Pair.map_fst (fn exp => Jatom exp))

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
              SOME _ => raise Fail "exp ident is infix"
            | _ => (exp, ctx))
        | Econstr {opp=false, id} =>
            (case Context.lookup_infix (longid_to_string id) ctx of
              SOME _ => raise Fail "exp constr is infix"
            | _ => (exp, ctx))
        | ( Enumber _
          | Estring _
          | Echar _
          | Eselect _
          | Eunit
          | Eident _
          | Econstr _ ) => (exp, ctx)
      end

    and transform_patrow ctx patrow =
      let open SMLSyntax in
        case patrow of
          PRlab {lab, pat} =>
            transform_pat ctx pat
            |> Pair.map_fst (fn pat => {lab=lab, pat=pat})
            |> Pair.map_fst PRlab
        | PRas {id, ty, aspat} =>
            let
              val (ty, ctx) =
                case ty of
                  SOME ty => transform_ty ctx ty |> Pair.map_fst SOME
                | NONE => (NONE, ctx)
              val (aspat, ctx) =
                case aspat of
                  SOME aspat => transform_pat ctx aspat |> Pair.map_fst SOME
                | NONE => (NONE, ctx)
            in
              (PRas {id=id, ty=ty, aspat=aspat}, ctx)
            end
        | PRellipsis => (patrow, ctx)
      end

    and transform_pat ctx pat =
      let open SMLSyntax in
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
            |> expand_node_with_ctx Ptuple pat
        | Papp {id, atpat} =>
            transform_pat ctx atpat
            |> Pair.map_fst (fn atpat => {id=id, atpat=atpat})
            |> expand_node_with_ctx Papp pat
        | Ptyped {pat, ty} =>
            let
              val (pat, ctx) = transform_pat ctx pat
              val (ty, ctx) = transform_ty ctx ty
            in
              ({pat=pat, ty=ty}, ctx)
              |> expand_node_with_ctx Ptyped pat
            end
        | Playered {opp, id, ty, aspat} =>
            let
              val (ty, ctx) =
                case ty of
                  SOME ty => transform_ty ctx ty |> Pair.map_fst SOME
                | NONE => (ty, ctx)
              val (aspat, ctx) = transform_pat ctx aspat
            in
              ({opp=opp, id=id, ty=ty, aspat=aspat}, ctx)
              |> expand_node_with_ctx Playered pat
            end
        | Pjuxta pjuxtas =>
            let
              fun transform_pjuxta ctx pjuxta =
                case pjuxta of
                  Jident (id, pat) => (Jident (id, pat), ctx)
                | Jatom pat =>
                    (case Node.getVal pat of
                      Pident _ => (Jatom pat, ctx)
                    | Pconstr _ => (Jatom pat, ctx)
                    | _ =>
                        transform_pat ctx pat
                        |> Pair.map_fst (fn pat => Jatom pat))

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
              SOME _ => raise Fail "pattern ident is infix"
            | _ => (pat, ctx))
        | Pconstr {opp=false, id} =>
            (case Context.lookup_infix (longid_to_string id) ctx of
              SOME _ => raise Fail "pattern constr is infix"
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

    and transform_ty ctx ty = (ty, ctx)

    and transform_fundec ctx fundec =
      let open SMLSyntax
        val funbinds = Node.getVal fundec in
        List.foldl
          (fn (funbind_node, (funbinds, ctx)) =>
            let
              val {id, arg_id, signat, body} = Node.getVal funbind_node
              val (signat, ctx) = transform_signat ctx signat
              val (body, ctx) = transform_module ctx body
            in
              (Node.map
                (Fn.const {id=id, arg_id=arg_id, signat=signat, body=body})
                funbind_node :: funbinds, ctx)
            end)
          ([], ctx)
          funbinds
        |> Pair.map_fst List.rev
        |> expand_node_with_ctx Fn.id fundec
      end

    and transform_sigdec ctx sigdec =
      let open SMLSyntax
          val sigbinds = Node.getVal sigdec in
        List.foldl
          (fn ({id, signat}, (sigbinds, ctx)) =>
            let
              val (signat, ctx) = transform_signat ctx signat
            in
              ({id=id, signat=signat}::sigbinds, ctx)
            end)
          ([], ctx)
          sigbinds
        |> Pair.map_fst List.rev
        |> expand_node_with_ctx Fn.id sigdec
      end

    and transform_signat ctx signat =
      let open SMLSyntax in
        case Node.getVal signat of
          Sspec specs =>
            List.foldl
              (fn (spec, (specs, ctx)) =>
                transform_spec ctx spec
                |> Pair.map_fst (fn spec => spec :: specs))
              ([], ctx)
              specs
            |> Pair.map_fst List.rev
            |> expand_node_with_ctx Sspec signat
        | Sident ident => (signat, ctx)
        | Swhere {signat, wheretypee={tyvars, id, ty}} =>
            let
              val (signat, ctx) = transform_signat ctx signat
              val (ty, ctx) = transform_ty ctx ty
            in
              ({signat=signat, wheretypee={tyvars=tyvars, id=id, ty=ty}}, ctx)
              |> expand_node_with_ctx Swhere signat
            end
      end

    and transform_condescs ctx condescs =
      List.foldl
        (fn ({id, ty}, (condescs, ctx)) =>
          case ty of
            NONE => ({id=id, ty=NONE}::condescs, ctx)
          | SOME ty =>
              transform_ty ctx ty |> Pair.map_fst (fn ty => {id=id, ty=SOME ty}::condescs))
        ([], ctx)
        condescs
      |> Pair.map_fst List.rev

    and transform_typdesc ctx {tyvars, tycon, ty, deriving} =
      case ty of
        NONE => ({tyvars=tyvars, tycon=tycon, ty=NONE, deriving=deriving}, ctx)
      | SOME ty =>
          transform_ty ctx ty
          |> Pair.map_fst
              (fn ty => {tyvars=tyvars, tycon=tycon, ty=SOME ty, deriving=deriving})

    and transform_spec ctx spec =
      let open SMLSyntax in
        case Node.getVal spec of
          SPval {id, ty} =>
            transform_ty ctx ty
            |> Pair.map_fst (fn ty => {id=id, ty=ty})
            |> expand_node_with_ctx SPval spec
        | SPtype typdesc =>
            transform_typdesc ctx typdesc
            |> expand_node_with_ctx SPtype spec
        | SPeqtype typdesc =>
            transform_typdesc ctx typdesc
            |> expand_node_with_ctx SPeqtype spec
        | SPdatdec {tyvars, tycon, condescs, deriving} =>
            transform_condescs ctx condescs
            |> Pair.map_fst (fn condescs =>
                {tyvars=tyvars, tycon=tycon, condescs=condescs, deriving=deriving})
            |> expand_node_with_ctx SPdatdec spec
        | SPexception {id, ty} =>
            let
              val (ty, ctx) =
                case ty of
                  NONE => (NONE, ctx)
                | SOME ty => transform_ty ctx ty |> Pair.map_fst SOME
            in
              ({id=id, ty=ty}, ctx)
            end
            |> expand_node_with_ctx SPexception spec
        | SPmodule {id, signat} =>
            transform_signat ctx signat
            |> Pair.map_fst (fn signat => {id=id, signat=signat})
            |> expand_node_with_ctx SPmodule spec
        | SPinclude signat =>
            transform_signat ctx signat
            |> expand_node_with_ctx SPinclude spec
        | SPsharing {specs, tycons} =>
            List.foldl
              (fn (spec, (specs, ctx)) =>
                transform_spec ctx spec
                |> Pair.map_fst (fn spec => spec::specs))
              ([], ctx)
              specs
            |> Pair.map_fst List.rev
            |> Pair.map_fst (fn specs => {specs=specs, tycons=tycons})
            |> expand_node_with_ctx SPsharing spec
        | ( SPdatrepl _ ) => (spec, ctx)
      end

    (* when entering a module, push a new scope
     * when exiting the module, take everything in the new scope and append a
     * `Module.` to it *)
    fun transform topdecs =
      let
        open SMLSyntax
        fun transform_topdec ctx topdec =
          case topdec of
            Strdec strdec =>
              transform_strdec ctx strdec
              |> Pair.map_fst Strdec
          | Sigdec sigdec =>
              transform_sigdec ctx sigdec
              |> Pair.map_fst Sigdec
          | Fundec fundec =>
              transform_fundec ctx fundec
              |> Pair.map_fst Fundec
        val (new_topdecs, _) =
          fold_transform
            transform_topdec
            Context.init
            topdecs
      in
        new_topdecs
      end
  end
