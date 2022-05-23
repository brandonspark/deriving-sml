(** Copyright (c) 2020 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

structure PrettyPrintAst :
sig
  val pretty: SMLSyntax.ast -> string

  val pretty_pat : SMLSyntax.pat -> string
end =
struct

  open SMLSyntax
  structure PD = PrettySimpleDoc
  open PD

  structure TC = TerminalColors

  val white = TC.hsv {h=38.0, s=0.0, v=0.75}
  val orange = TC.hsv {h=35.0, s=0.79, v=0.9}
  val green = TC.hsv {h=128.0, s=0.43, v=0.9}
  val heavygreen = TC.hsv {h=117.0, s=1.0, v=0.26}
  val blue = TC.hsv {h=220.0, s=0.75, v=0.85}
  val heavyblue = TC.hsv {h=222.0, s=0.95, v=0.85}
  val lightblue = TC.hsv {h=200.0, s=0.45, v=0.83}
  val yellow = TC.hsv {h=61.0, s=0.45, v=0.80}
  val purple = TC.hsv {h=269.0, s=0.52, v=1.0}
  val pink = TC.hsv {h=300.0, s=0.61, v=0.9}
  val red = TC.hsv {h=0.0, s=0.72, v=0.8}
  val heavyred = TC.hsv {h=0.0, s=0.92, v=0.9}

  infix 2 ++ +-+ $$ // $$< //< ^^
  fun x ++ y = beside (x, y)
  fun x +-+ y = x ++ space ++ y
  fun x $$ y = aboveOrSpace (x, y)
  fun x $$< y = y $$ x
  fun x // y = aboveOrBeside (x, y)
  fun x //< y = y // x

  val text_syntax = text lightblue
  val text_literal = text pink
  val text_lab = text blue

  fun spaces n =
    List.foldl op++ empty (List.tabulate (n, fn _ => space))

  fun parensAround (x: doc) =
    text_syntax "(" ++ x ++ text_syntax ")"

  fun separateWithSpaces (items: doc option list) : doc =
    let
      val items: doc list = List.mapPartial (fn x => x) items
    in
      case items of
        [] => empty
      | first :: rest =>
          List.foldl (fn (next, prev) => prev +-+ next) first rest
    end

  (* if all is true: apply delim to all elements, including first.
   * delim is prepended to any mapped elements. *)
  fun apply_list_base f delim all l =
    case l of
      [] => []
    | first::rest =>
        (if all then text_syntax delim ++ f first else f first)
        ::
        List.map (fn elem => text_syntax delim ++ f elem) rest

  fun apply_list_after_base f delim l =
    case l of
      [] => []
    | _ =>
      List.map (fn elem => f elem ++ text_syntax delim) (ListUtils.up_to_last l)
      @ [f (ListUtils.last l)]
      (* Yeah, I know. *)


  (* Put the list of documents together in the right order. *)
  fun combine_list space smush [] = text_syntax ""
    | combine_list space smush (first::rest) =
    let
      val result = List.foldl (if space then op$$< else op//<) first rest
    in
      if smush then group result else result
    end

  fun show_list_base smush f delim all space l =
    combine_list space smush (apply_list_base f delim all l)

  (* show_list by default calls show_list_base with these settings:
   * - Group the combined list
   * - Do not newline-separate
   * - Delimiter is applied to not all elements *)
  fun show_list f delim l = show_list_base true f delim false false l

  (* Delim is put after all but the last element, instead of before all but the
   * first. *)
  fun show_list_after f delim l =
    case l of
      [] => text_syntax ""
    | [elem] => f elem
    | _ =>
      let
        val prelude_mapped =
          List.map (fn elem => f elem ++ text_syntax delim) (ListUtils.up_to_last l)
      in
        group (List.foldr op// (f (ListUtils.last l)) prelude_mapped)
      end

  (* Suppose we have a list L : 'a list.
   * Suppose we also have a function mk : 'a -> bool -> doc.
   * Then we want to $$ all of the elements in the list together, where
   * `mk true` is applied to the first element, and `mk false` is applied to the
   * rest.
   * That's what this function does. *)
  fun show_list_mk mk l =
    List.foldl op$$<
      (mk true (List.nth (l, 0)))
      (List.map (mk false) (List.drop (l, 1)))

  (* Suppose we would like to show all the elements in a list with some
   * delimiter, in such a way that the first element is prepended with one
   * (spaced) string, and then all after are prepended with a different string.
   * That's what this does. *)
  fun show_list_prepend color smush first after f delim l =
    combine_list true smush (
      (ListUtils.map_cons
        (Fn.curry (op +-+) (text color first))
        (Fn.curry (op +-+) (text color after))
        (apply_list_base f delim false l))
    )

  fun show_symbol color = text color o Symbol.toValue
  fun show_symbol_node color = text color o Symbol.toValue o Node.getVal

  fun show_char c = text_literal ("#\"" ^ Char.toString c ^ "\"")

  fun show_id color identifier = (show_symbol_node color) identifier

  fun show_longid color longid =
    let
      val mapped =
        List.mapi
          (fn (i, elem) =>
            if i = List.length longid - 1 then
              show_symbol_node color elem
            else
              show_symbol_node orange elem)
          longid
    in
      List.foldl
        (fn (elem, acc) => acc ++ text_syntax "." ++ elem)
        (List.nth (mapped, 0))
        (List.drop (mapped, 1))
    end

  fun show_juxta show_fn juxta =
    case juxta of
      Jident (_, atom) => show_fn atom
    | Jatom atom => show_fn atom

  val show_tyvar = show_id yellow

  fun show_tyvars tyvars =
    parensAround (
      show_list show_tyvar ", " tyvars
    )
  fun show_tyvars_option [] = NONE
    | show_tyvars_option [tyvar] = SOME (show_tyvar tyvar)
    | show_tyvars_option other =  SOME (show_tyvars other)


  fun show_opp opp = if opp then text_syntax "op" else text_syntax ""

  fun bool_to_option b default =
    if b then SOME default
    else NONE

  fun show_setting (name, value) =
    group (
      show_id blue name ++ text_syntax "="
      $$
      show_id white value
    )
  fun show_settings settings =
    text_syntax "{" ++ show_list show_setting ", " settings ++ text_syntax "}"

  fun show_plugin (name, settings) =
    case settings of
      [] => show_id white name
    | _ => group (show_id white name $$ show_settings settings)
  fun show_plugins plugins =
    show_list show_plugin "," plugins

  fun show_deriving deriving =
    case deriving of
      NONE => text white ""
    | SOME plugins =>
      group (
        text_syntax "[" ++ text pink ".deriving"
        $$
        show_plugins plugins
        ++
        text_syntax "]"
      )

  local
    open SMLSyntax
    val color = green
    val show_tyvar = show_id yellow
    val show_id = show_id color
    val show_longid = show_longid color
  in
    fun show_ty ty = show_ty_ (Node.getVal ty)
    and show_ty_ ty_ =
      case ty_ of
        Tident longid =>
        show_longid longid
      | Ttyvar id =>
        show_tyvar id
      | Tapp (typarams, longid) =>
        (case typarams of
          [elem] => show_ty elem
        | _ =>
          text_syntax "(" ++ show_list show_ty ", " typarams ++ text_syntax ")")
        +-+
        show_longid longid
      | Tprod tys =>
        text_syntax "(" ++ show_list show_ty " * " tys ++ text_syntax ")"
      | Tarrow (t1, t2) =>
        group (
          parensAround (
            show_ty t1 +-+ text_syntax "->"
            $$
            show_ty t2
          )
        )
      | Trecord fields =>
        let
          val fields_doc =
            List.map
              (fn {lab, ty} =>
                group (
                  show_symbol_node blue lab +-+ text_syntax ":"
                  $$
                  spaces 2 ++ show_ty ty
                ))
              fields
        in
          group (
            text_syntax "{"
            ++
            show_list_after Fn.id ", " fields_doc
            ++
            text_syntax "}"
          )
        end
  end

  local
    open SMLSyntax
    val color = red
    val show_id = show_id color
    val show_constr = show_longid heavyred
    val show_longid = show_longid color
    val text = text color
  in
    fun show_pat pat = show_pat_ (Node.getVal pat)
    and pretty_pat pat = PrettySimpleDoc.toString (show_pat pat)
    and show_pat_ pat_ =
      case pat_ of
        Pnumber n => text_literal (Int.toString n)
      | Pword s => text_literal ("0w" ^ Symbol.toValue s)
      | Pstring s => text_literal ("\"" ^ Symbol.toValue s ^ "\"")
      | Pchar c => show_char c
      | Pwild => text "_"
      | Pident {opp, id} =>
          if opp then
            parensAround (
              text_syntax "op"
              +-+
              show_id id
            )
          else
            show_id id
      | Pconstr {opp, id} =>
          if opp then
            parensAround (
              text_syntax "op"
              +-+
              show_constr id
            )
          else
            show_constr id
      | Precord patrows =>
          text_syntax "{" ++ show_list_after show_patrow ", " patrows ++ text_syntax "}"
      | Punit => text "()"
      | Ptuple pats =>
          parensAround (show_list_after show_pat ", " pats)
      | Plist pats =>
          text_syntax "[" ++ show_list show_pat ", " pats ++ text_syntax "]"
      | Papp {id, atpat} =>
          parensAround (
            show_constr id
            +-+
            show_pat atpat
          )
      | Ptyped {pat, ty} =>
          parensAround (
            show_pat pat
            +-+
            text_syntax ":"
            +-+
            show_ty ty
          )
      | Playered {opp, id, ty, aspat} =>
          parensAround (
            separateWithSpaces
              [ bool_to_option opp (text_syntax "op")
              , SOME (show_id id)
              , Option.map show_ty ty
              , SOME (text_syntax "as")
              , SOME (show_pat aspat) ]
          )
      | Pjuxta pats =>
          text_syntax "(" ++ show_list (show_juxta show_pat) " " pats ++ text_syntax ")"

    and show_patrow patrow =
      case patrow of
        PRellipsis => text "..."
      | PRlab {lab, pat} =>
          group (
            show_symbol_node blue lab +-+ text_syntax "="
            $$
            show_pat pat
          )
      | PRas {id, ty, aspat} =>
          separateWithSpaces
            [ SOME (show_id id)
            , Option.map (fn ty => text_syntax ":" +-+ show_ty ty) ty
            , Option.map (fn pat => text_syntax "as" +-+ show_pat pat) aspat ]
  end

  fun show_conbind {opp, id, ty} =
    separateWithSpaces
      [ bool_to_option opp (text_syntax "op")
      , SOME (show_id heavyblue id)
      , Option.map (fn ty => text_syntax "of" +-+ show_ty ty) ty ]

  local
    open SMLSyntax
  in
    val show_number = fn
      Int i => text_literal (Int.toString i)
    | Word s => text_literal ("0w" ^ s)
    | Real r => text_literal (Real.toString r)

    fun show_exp exp = show_exp_ (Node.getVal exp)
    and show_exp_ exp_ =
      let
        val color = white
        val show_id = show_id color
        val show_constr = show_longid heavyblue
        val show_longid = show_longid color
        val text = text color
      in
      case exp_ of
        Enumber n => show_number n
      | Estring s => text_literal ("\"" ^ Symbol.toValue s ^ "\"")
      | Echar c => show_char c
      | Erecord fields =>
          text_syntax "{"
          ++
          show_list_after
            (fn {lab, exp} =>
              separateWithSpaces
                [ SOME (show_symbol_node blue lab)
                , SOME (text_syntax "=")
                , SOME (show_exp exp) ] )
            ", "
            fields
          ++
          text_syntax "}"
      | Eselect lab =>
          text_syntax "#" ++ show_symbol_node blue lab
      | Eunit => text "()"
      | Eident {opp, id} =>
          if opp then
            parensAround (text_syntax "op" +-+ show_longid id)
          else
            show_longid id
      | Econstr {opp, id} =>
          if opp then
            parensAround (text_syntax "op" +-+ show_constr id)
          else
            show_constr id
      | Etuple exps =>
          group (parensAround (show_list_after show_exp ", " exps))
      | Elist exps =>
          group (text_syntax "[" ++ show_list_after show_exp ", " exps ++ text_syntax "]")
      | Eseq exps =>
          group (
            parensAround (
              show_list_after show_exp "; " exps
            )
          )
      | Elet {dec, exps} =>
          group (
            text_syntax "let"
            $$
            spaces 2 ++ show_dec dec
            $$
            text_syntax "in"
            $$
            spaces 2 ++ show_list show_exp "; " exps
            $$
            text_syntax "end"
          )
      | Eapp {left, right} =>
          parensAround (
            show_exp left +-+ show_exp right
          )
      | Etyped {exp, ty} =>
          parensAround (
            show_exp exp +-+ text_syntax ":" +-+ show_ty ty
          )
      | Eandalso {left, right} =>
          parensAround (
            show_exp left +-+ text_syntax "andalso" +-+ show_exp right
          )
      | Eorelse {left, right} =>
          parensAround (
            show_exp left +-+ text_syntax "orelse" +-+ show_exp right
          )
      | Ehandle {exp, matches} =>
          parensAround (
            show_exp exp +-+ text_syntax "handle"
            $$
            show_list show_match "| " matches
          )
      | Eraise exp =>
          parensAround (
            text_syntax "raise" +-+ show_exp exp
          )
      | Eif {exp1, exp2, exp3} =>
          group (
            parensAround (
              group (
                text_syntax "if"
                $$
                show_exp exp1
                $$
                text_syntax "then"
                $$
                show_exp exp2
              )
              $$
              group (
                text_syntax "else"
                $$
                show_exp exp3
              )
            )
          )
      | Ewhile {exp1, exp2} =>
          group (
            parensAround (
              group (
                text_syntax "while"
                $$
                show_exp exp1
              )
              $$
              group (
                text_syntax "do"
                $$
                show_exp exp2
              )
            )
          )
      | Ecase {exp, matches} =>
          group (
            parensAround (
              group(
                text_syntax "case"
                $$
                show_exp exp
                $$
                text_syntax "of"
              )
              $$
              spaces 2 ++ show_match (List.nth (matches, 0))
              $$
              show_list_base false show_match "| " true true (List.drop (matches, 1))
            )
          )
      | Efn matches =>
          let
            val inner = group (text_syntax "fn" +-+ show_match (List.nth (matches, 0)))
          in
            group (
              parensAround (
                if List.length matches = 1 then
                  inner
                else
                inner $$
                space ++ show_list_base false show_match "| " true true (List.drop (matches, 1))
              )
            )
          end
      | Ejuxta exps =>
          group (
            parensAround (
              show_list (show_juxta show_exp) " " exps
            )
          )
      end

    and show_match {pat, exp} =
      group (
        show_pat pat +-+ text_syntax "=>"
        $$
        spaces 2 ++ show_exp exp
      )
    and show_match_equal {pat, exp} =
      group (
        show_pat pat +-+ text_syntax "="
        $$
        spaces 2 ++ show_exp exp
      )
    and show_dec dec = show_dec_ (Node.getVal dec)
    and show_dec_ dec_ =
      let
        val rec_color = yellow
        val text_rec = text rec_color

        val color = purple
        val show_id = show_id white
        val show_open = show_longid orange
        val show_longid = show_longid white
        val text = text color

        fun show_exbind exbind =
          case exbind of
            Xnew {opp, id, ty} =>
              if opp then
                separateWithSpaces
                  [ SOME (text_syntax "op")
                  , SOME (show_id id)
                  , Option.map (fn ty => text_syntax "of" +-+ show_ty ty) ty ]
              else
                (case ty of
                  NONE => show_id id
                | SOME ty =>
                    show_id id +-+ text_syntax "of" +-+ show_ty ty)
          | Xrepl {left_opp, left_id, right_opp, right_id} =>
              separateWithSpaces
                [ bool_to_option left_opp (text_syntax "op")
                , SOME (show_id left_id)
                , SOME (text_syntax "=")
                , bool_to_option right_opp (text_syntax "op")
                , SOME (show_longid right_id) ]

        fun show_typbind {tyvars, tycon, ty, deriving} =
          separateWithSpaces
            [ show_tyvars_option tyvars
            , SOME (show_id tycon)
            , SOME (text_syntax "=")
            , SOME (show_ty ty)
            , SOME (show_deriving deriving)
            ]

        fun show_datbind str mark {tyvars, tycon, conbinds, deriving} =
          group (
            separateWithSpaces
              [ SOME (text (if mark then str else "and"))
              , show_tyvars_option tyvars
              , SOME (show_id tycon)
              , SOME (text_syntax "=") ]
            $$
            (spaces 2 ++ (show_conbind (List.nth (conbinds, 0))))
            $$
            show_list_prepend color false "|" "|" show_conbind "" (List.drop (conbinds, 1))
            $$
            show_deriving deriving
          )
      in
      case dec_ of
        Dval {recc, tyvars, valbinds} =>
          List.foldr op$$<
          (group (
            separateWithSpaces
              [ SOME (text "val")
              , show_tyvars_option tyvars
              , bool_to_option recc (text_rec "rec")
              , SOME (show_match_equal (List.nth (valbinds, 0))) ]
          ))
          (List.map
            (fn match => text "and" +-+ show_match_equal match)
            (List.drop (valbinds, 1)))
      | Dfun {tyvars, fvalbinds} =>
          let
            fun mk { opp, id, pats, ty, exp } =
              group (
                separateWithSpaces
                  [ bool_to_option opp (text_syntax "op")
                  , SOME (show_id id)
                  , SOME (show_list show_pat " " pats)
                  , Option.map (fn ty => text_syntax ":"  +-+ show_ty ty) ty
                  , SOME (text_syntax "=") ]
                $$
                show_exp exp
              )
            fun mk_fun mark clauses =
                List.foldr op$$<
                  (group (
                    text (if mark then "fun" else "and")
                    +-+
                    mk (List.nth (clauses, 0))
                  ))
                  (List.map
                    (fn elem => spaces 2 ++ text_syntax "|" +-+ mk elem)
                    (List.drop (clauses, 1))
                  )
            val fun_docs =
              show_list_mk mk_fun fvalbinds
          in
            group (fun_docs)
          end
      | Dtype typbinds =>
          show_list_prepend color false "type" "and" show_typbind "" typbinds
      | Ddatdec {datbinds, withtypee} =>
          let
            val datdecs =
              show_list_mk (show_datbind "datatype") datbinds
          in
            case withtypee of
              NONE => datdecs
            | SOME typbinds =>
                group (
                  datdecs
                  $$
                  group (
                    show_list_prepend color false "withtype" "and" show_typbind "" typbinds
                  )
                )
          end
      | Ddatrepl {left_tycon, right_tycon} =>
          separateWithSpaces
            [ SOME (text "datatype")
            , SOME (show_id left_tycon)
            , SOME (text_syntax "=")
            , SOME (text "datatype")
            , SOME (show_longid right_tycon) ]
      | Dabstype {datbinds, withtypee, withh} =>
          let
            val typdecs =
              show_list_mk (show_datbind "abstype") datbinds
            val withh =
              group (
                text_syntax "with"
                $$
                show_dec withh
                $$
                text_syntax "end"
              )
          in
            case withtypee of
              NONE =>
                group (typdecs $$ withh)
            | SOME typbinds =>
                group (
                  typdecs
                  $$
                  show_list_prepend color false "withtype" "and" show_typbind "" typbinds
                  $$
                  withh
                )
          end
      | Dexception exbinds =>
          group (
            show_list_prepend color false "exception" "and" show_exbind "" exbinds
          )
      | Dlocal {left_dec, right_dec} =>
          group (
            text_syntax "local"
            $$
            spaces 2 ++ show_dec left_dec
            $$
            text_syntax "in"
            $$
            spaces 2 ++ show_dec right_dec
            $$
            text_syntax "end"
          )
      | Dopen longids =>
          text_syntax "open" +-+ show_list show_open " " longids
      | Dseq decs =>
          show_list_base false show_dec "" false true decs
      | Dinfix {precedence, ids} =>
          separateWithSpaces
            [ SOME (text_syntax "infix")
            , Option.map (fn n => text_literal (Int.toString n)) precedence
            , SOME (show_list show_id " " ids) ]
      | Dinfixr {precedence, ids} =>
          separateWithSpaces
            [ SOME (text_syntax "infixr")
            , Option.map (fn n => text_literal (Int.toString n)) precedence
            , SOME (show_list show_id " " ids) ]
      | Dnonfix ids =>
          text_syntax "nonfix"
          ++
          show_list show_id " " ids
      | Dempty => text_syntax ""
      end
  end

  local
    open SMLSyntax
  in
    fun show_strdec strdec = show_strdec_ (Node.getVal strdec)
    and show_strdec_ strdec_ =
      let
        val color = orange
        val show_id = show_id color
        val show_longid = show_longid color
      in
      case strdec_ of
        DMdec dec => show_dec dec
      | DMstruct body =>
          let
            fun mk mark {id, seal, module} =
              group (
                group (
                  separateWithSpaces
                    [ SOME (if mark then text_syntax "structure" else text_syntax "and")
                    , SOME (show_id id)
                    , Option.map (fn {opacity, signat} =>
                        case opacity of
                          Transparent => text_syntax ":" +-+ show_signat signat
                        | Opaque => text_syntax ":>" +-+ show_signat signat) seal
                    , SOME (text_syntax "=") ]
                )
                $$
                spaces 2 ++ show_module module
              )
          in
            show_list_mk mk body
          end
      | DMlocal {left_dec, right_dec} =>
          group (
            text_syntax "local"
            $$
            spaces 2 ++ show_strdec left_dec
            $$
            text_syntax "in"
            $$
            spaces 2 ++ show_strdec right_dec
            $$
            text_syntax "end"
          )
      | DMseq strdecs =>
          show_list_mk (Fn.const show_strdec) strdecs
      | DMempty => text_syntax ""
      end
    and show_module module = show_module_ (Node.getVal module)
    and show_module_ module_ =
      let
        val color = orange
        val show_id = show_id color
        val show_longid = show_longid color
      in
      case module_ of
        Mident longid => show_longid longid
      | Mstruct strdec =>
          group (
            text_syntax "struct"
            $$
            spaces 2 ++ show_strdec strdec
            $$
            text_syntax "end"
          )
      | Mseal {module, opacity, signat} =>
          separateWithSpaces
            [ SOME (show_module module)
            , SOME (case opacity of Transparent => text_syntax ":" | _ => text_syntax ":>")
            , SOME (show_signat signat) ]
      | Mapp {functorr, module} =>
          group (
            show_id functorr +-+ text_syntax "("
            ++
            show_module module
            ++
            text_syntax ")"
          )
      | Mlet {dec, module} =>
          group (
            text_syntax "let"
            $$
            spaces 2 ++ show_strdec dec
            $$
            text_syntax "in"
            $$
            spaces 2 ++ show_module module
            $$
            text_syntax "end"
          )
      end
    and show_signat signat = show_signat_ (Node.getVal signat)
    and show_signat_ signat_ =
      let
        val color = orange
        val show_id = show_id color
        val show_longid = show_longid color
      in
      case signat_ of
        Sspec specs =>
          group (
            text_syntax "sig"
            $$
            spaces 2 ++ show_list_base false show_spec "" true true specs
            $$
            text_syntax "end"
          )
      | Sident id =>
          show_id id
      | Swhere {signat, wheretypee={tyvars, id, ty}} =>
          group (
            show_signat signat
            $$
            text_syntax "where type"
            $$
            group (
              separateWithSpaces
                [ show_tyvars_option tyvars
                , SOME (show_longid id) ]
                $$
                show_ty ty
            )
          )
        end
    and show_spec spec = show_spec_ (Node.getVal spec)
    and show_spec_ spec_ =
      let
        val color = purple
        val text = text color
        val show_id = show_id white
        val show_longid = show_longid white
        fun show_condesc {id, ty} =
          separateWithSpaces
            [ SOME (show_id id)
            , Option.map (fn ty => text_syntax "of" +-+ show_ty ty) ty ]
        fun show_typdesc {tyvars, tycon, ty, deriving} =
          separateWithSpaces
            [ show_tyvars_option tyvars
            , SOME (show_id tycon)
            , Option.map (fn ty => text_syntax "=" +-+ show_ty ty) ty
            , SOME (show_deriving deriving)
            ]

        fun show_datbind str mark {tyvars, tycon, conbinds, deriving} =
          group (
            separateWithSpaces
              [ SOME (text (if mark then str else "and"))
              , show_tyvars_option tyvars
              , SOME (show_id tycon)
              , SOME (text_syntax "=") ]
            $$
            (spaces 2 ++ (show_conbind (List.nth (conbinds, 0))))
            $$
            show_list_prepend color false "|" "|" show_conbind "" (List.drop (conbinds, 1))
            $$
            show_deriving deriving
          )
      in
      case spec_ of
        SPval {id, ty} =>
          group (
            separateWithSpaces
              [ SOME (text "val")
              , SOME (show_id id)
              , SOME (text_syntax ":") ]
            $$
            spaces 2 ++ show_ty ty
          )
      | SPtype typdesc =>
          show_typdesc typdesc
      | SPeqtype typdesc =>
          show_typdesc typdesc
      | SPdatdec {tyvars, tycon, condescs, deriving} =>
          let
            fun mk {tyvars, tycon, condescs} =
              group (
                separateWithSpaces
                  [ SOME (text "datatype")
                  , show_tyvars_option tyvars
                  , SOME (show_id tycon)
                  , SOME (text_syntax "=") ]
                $$
                show_list show_condesc "| " condescs
                $$
                show_deriving deriving
              )
          in
            (* Conbinds look like condescs, but with an extra "opp".
             * Elaborate the condescs to degenerate conbinds to make things
             * easier.
             *)
              show_datbind "datatype" true
                {tyvars=tyvars, tycon=tycon, conbinds=List.map
                  (fn {id, ty} => {opp=false, id=id, ty=ty}) condescs
                , deriving = deriving
                }
          end
      | SPdatrepl {left_tycon, right_tycon} =>
          separateWithSpaces
            [ SOME (text "datatype")
            , SOME (show_id left_tycon)
            , SOME (text_syntax "=")
            , SOME (text "datatype")
            , SOME (show_longid right_tycon) ]
      | SPexception {id, ty} =>
          separateWithSpaces
            [ SOME (text "exception")
            , SOME (show_id id)
            , Option.map (fn ty => text_syntax "of" +-+ show_ty ty) ty ]
      | SPmodule {id, signat} =>
          separateWithSpaces
            [ SOME (text "structure")
            , SOME (show_id id)
            , SOME (text_syntax ":")
            , SOME (show_signat signat) ]
      | SPinclude signat =>
          text_syntax "include" +-+ show_signat signat
      | SPsharing {specs, tycons} =>
          group (
            show_list_base false show_spec "" true true specs
            $$
            spaces 2 ++ text_syntax "sharing type"
            $$
            group (spaces 2 ++ show_list show_longid " = " tycons)
          )
      end
    and show_sigbinds sigbinds =
      let
        val color = orange
        val text = text color
        val show_id = show_id color
        val show_longid = show_longid color
        fun mk mark {id, signat} =
          group (
            separateWithSpaces
              [ SOME (if mark then text_syntax "signature" else text "and")
              , SOME (show_id id)
              , SOME (text_syntax "=") ]
            $$
            spaces 2 ++ show_signat signat
          )
      in
        show_list_mk mk sigbinds
      end

    and show_sigdec sigdec = show_sigbinds (Node.getVal sigdec)
  end



  fun show_funbinds binds =
    let
      val color = orange
      val show_id = show_id color
      val show_longid = show_longid color
      val text = text white

      fun mk mark node =
        let
          val {id, arg_id, signat, body} = Node.getVal node
        in
          group (
            separateWithSpaces
              [ SOME (if mark then text "functor" else text "and")
              , SOME (show_id id)
              , SOME (parensAround (
                  show_id arg_id +-+ text_syntax ":" +-+ show_signat signat)
                )
              , SOME (text_syntax "=") ]
            $$
            show_module body
          )
        end
    in
      show_list_mk mk binds
    end

  fun show_fundec node = show_funbinds (Node.getVal node)

  local
    open SMLSyntax
  in
    fun show_topdec topdec =
      case topdec of
        Strdec strdec => show_strdec strdec
      | Sigdec sigdec => show_sigdec sigdec
      | Fundec fundec => show_fundec fundec

    fun show_ast ast =
      show_list_base false show_topdec "" false true ast
  end

  fun pretty ast = PrettySimpleDoc.toString (show_ast ast)

end
