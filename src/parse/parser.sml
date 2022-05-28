
type result = SMLSyntax.ast * Token.token list
signature PARSER =
  sig
    val parse : char StreamStreamable.t -> (string list, result) Either.either

    val parse_string : string -> (string list, result) Either.either

    val parse_file : string -> (string list, result) Either.either

    val parse_file_transformed : string -> (string list, result) Either.either

    val parse_file_to_string : string -> string

    val parse_file_transformed_to_string : string -> string
  end

structure Parser :> PARSER =
  struct
    open SMLSyntax
    open Error

    type span = Span.span

    structure S = Stream

    type symbol = Symbol.symbol

    fun identity x = x
    fun null () = []
    fun sing x = [x]
    fun pair (x, y) = [x, y]
    fun sing_span (x, span) = Node.create

    val cons_sym = Symbol.fromValue "::"
    val equal_sym = Symbol.fromValue "="
    val times_sym = Symbol.fromValue "*"
    val ref_sym = Symbol.fromValue "ref"
    val lparen_sym = Symbol.fromValue "("
    val rparen_sym = Symbol.fromValue ")"

    val option_to_bool = fn
      SOME _ => true
    | NONE => false

    (* Naive N^2 solution. Can improve this later. *)
    fun all_unique eq l =
      case l of
        [] => ()
      | x::xs =>
          if (not (List.exists (fn y => eq (x, y)) xs)) then
            all_unique eq xs
          else
            raise Fail "not all unique elements in list"

    fun nyi () = ()
    fun assert_fvalbinds_valid _ = nyi ()
    fun assert_valbinds_valid _ _ = nyi ()
    fun assert_valid_precedence _ = nyi ()

    structure Arg =
      struct
        datatype terminal = datatype Token.token

        type identifier = identifier
        type identifiers = identifier list
        type int_span = int Node.t
        type string_span = string Node.t
        type char_span = char Node.t
        type real_span = real Node.t
        type span = span

        type optional = span option

        type ty     = ty
        type exp    = exp
        type pat    = pat
        type dec    = dec
        type signat = signat
        type module = module
        type spec   = spec

        (* Identifiers *)
        type nothing = unit
        type longid = longid
        type longids = longid list
        type idents = identifier list

        val identity = identity

        val cons_idents = op ::
        val sing_idents = sing
        val cons_longid = op ::
        val sing_longid = sing
        val pair_longids = pair
        val cons_longids = op ::

        type label = symbol Node.t
        fun ident_label id = id
        fun number_label node =
          Node.map
            (fn i => Symbol.fromValue (Int.toString i))
            node

        val ref_ident = Node.create2 ref_sym
        val equal_ident = Node.create2 equal_sym
        val times_ident = Node.create2 times_sym
        val id_ident = identity
        val sing_identifiers = sing
        val cons_identifiers = op ::

        fun optional_false () = NONE
        fun optional_true span = SOME span

        (********************)
        (* DERIVING SECTION *)
        (********************)

        type setting = setting
        type settings = settings
        val mk_setting = identity
        val sing_settings = sing
        val cons_settings = op ::

        type plugin = plugin
        type plugins = plugins
        fun bare_plugin name = (name, [])
        val settings_plugin = identity
        val sing_plugins = sing
        val cons_plugins = op ::

        type optional_deriving = plugins option
        fun none_deriving _ = NONE
        fun some_deriving (name, plugins) =
          case Symbol.toValue (Node.getVal name) of
            "deriving" => SOME plugins
          | s =>
              err
                (ExpectedIdent
                  { expected = "deriving"
                  , got = s
                  , span =
                      case plugins of
                        [] => Node.getSpan name
                      | _ =>
                        Span.join
                          (Node.getSpan name)
                          (case ListUtils.last plugins of
                            (id, []) => Node.getSpan id
                          | (_, l) => Node.getSpan ((fn (_, y) => y) (ListUtils.last l))
                          )
                  }
                )

        (***************)
        (* EXP SECTION *)
        (***************)

        (* Exprows *)
        type exprow = {lab : label, exp : exp}
        type exprows = exprow list

        fun mk_exprow {lab, exp} = {lab=lab, exp=exp}
        val id_exprows = identity
        val nil_exprows = null
        val sing_exprows = sing
        val cons_exprows = op ::

        (* Exp sequences. *)
        type exps = exp list
        val id_exps = identity
        val nil_exps = null
        val sing_exps = sing
        val cons_exps = op ::
        val pair_exps = pair

        (* Atomic exps. *)
        local
          open SMLSyntax
        in
          val num_exp = Node.map (fn n => Enumber (Int n))
          val word_exp = Node.map (fn s => Enumber (Word s))
          val real_exp = Node.map (fn r => Enumber (Real r))

          val str_exp = Node.map (fn s => Estring (Symbol.fromValue s))
          val char_exp = Node.map (fn c => Echar c)

          val op_var_exp = fn
            (NONE, longid) =>
              Node.create (Eident {opp=false, id=longid}, Node.list_span longid)
          | (SOME left_span, longid) =>
              Node.create (Eident {opp=true, id=longid},
                           Node.join_spanl left_span (ListUtils.last longid))
          val op_constr_exp = fn
            (NONE, longid) =>
              Node.create (Econstr {opp=false, id=longid},
                           Node.list_span longid)
          | (SOME left_span, longid) =>
              Node.create (Econstr {opp=true, id=longid},
                           Node.join_spanl left_span (ListUtils.last longid))

          fun record_exp {left, exprows, right} =
            case exprows of
              [] => Node.create (Eunit, Span.join left right)
            | _ =>  Node.create (Erecord exprows, Span.join left right)
          fun select_exp (left_span, lab) =
            Node.create (Eselect lab, Span.join left_span (Node.getSpan lab))
          fun tuple_exp {left, exps, right} =
            case exps of
              [] => Node.create (Eunit, Span.join left right)
            | [exp] => Node.create (Node.getVal exp, Span.join left right)
            | other => Node.create (Etuple other, Span.join left right)
          fun deriving_exp {left, plugin, ty, right} =
            (* TODO: if the type doesn't exist, you're gonna have a bunch of
             * nonsense
             *)
            Derive.derive_exp plugin ty
          fun list_exp {left, exps, right} =
            Node.create (Elist exps, Span.join left right)
          fun nil_exp {left, right} =
            Node.create (Elist [], Span.join left right)
          fun seq_exp {left, expseq, right} =
            Node.create (Eseq expseq, Span.join left right)
          fun let_exp {left, dec, exps, right} =
            Node.create (Elet {dec=dec, exps=exps}, Span.join left right)
        end

        (* Matches. *)
        type matchrule = {pat : pat, exp : exp}
        type match = matchrule list
        fun mk_matchrule {pat, exp} = {pat=pat, exp=exp}
        val sing_match = sing
        val cons_match = op ::

        local
          open SMLSyntax
        in
          (* Ejuxtas. *)
          type ejuxta = exp
          type ejuxtas = exp juxta list
          val atom_ejuxta = identity
          val sing_ejuxtas = identity
          fun mk_ejuxta exp =
            case Node.getVal exp of
              Eident {id=[single], opp=false} =>
                Jident (single, exp)
            | Econstr {opp=false, id=[single]} =>
                Jident (single, exp)
            | _ => Jatom exp
          fun pair_ejuxtas {left_exp, right_exp} =
            [ mk_ejuxta left_exp, mk_ejuxta right_exp ]
          fun cons_ejuxtas {exp, ejuxtas} =
            mk_ejuxta exp :: ejuxtas
          fun ejuxta_span (Jident (_, node)) = Node.getSpan node
            | ejuxta_span (Jatom node) = Node.getSpan node
          (*
          fun cons_ejuxtas {left_exp, right_exp} =
            let
              val new_span = Node.join_span left_exp right_exp
              val new_ejuxta =
                case Node.getVal left_exp of
                  Eident {id=[single], opp=false} => Jident (single, left_exp)
                  (* Need this and below because of :: *)
                | Econstr {id=[single], opp=false} => Jident (single, left_exp)
                | _ => Jatom left_exp
            in
              case Node.getVal right_exp of
                Ejuxta [juxta] => (* Should be impossible. *)
                  Node.create ( Ejuxta [new_ejuxta, juxta], new_span )
              | Eident {id=longid, opp} =>
                  (case longid of
                    [single] =>
                      Node.create
                        (Ejuxta
                          [ new_ejuxta,
                            Jident (single,right_exp) ],
                          new_span)
                  | _ =>
                      Node.create (Ejuxta [new_ejuxta, Jatom right_exp], new_span))
              | Econstr {opp, id=[single]} =>
                  Node.create
                    (Ejuxta [ new_ejuxta, Jident (single, right_exp) ], new_span)
              | Ejuxta juxtas =>
                  Node.create ( Ejuxta (new_ejuxta::juxtas) , new_span )
              | _ =>
                  Node.create ( Ejuxta [new_ejuxta, Jatom right_exp], new_span )
            end *)

          (* Exps. *)
          fun ejuxtas_exp ejuxtas =
            Node.create (Ejuxta ejuxtas, Span.join_list (List.map ejuxta_span ejuxtas))
          val id_exp = identity
          fun typed_exp   (input as {exp, ty}) =
            Node.create (Etyped   input, Node.join_span exp ty)
          fun andalso_exp (input as {left, right}) =
            Node.create (Eandalso input, Node.join_span left right)
          fun orelse_exp  (input as {left, right}) =
            Node.create (Eorelse  input, Node.join_span left right)
          fun handle_exp  (input as {exp, matches}) =
            Node.create (Ehandle  input, Node.join_span exp (#pat (ListUtils.last matches)))
          fun raise_exp {left, exp} =
            Node.create (Eraise exp, Span.join left (Node.getSpan exp))
          fun if_exp {left, exp1, exp2, exp3} =
            Node.create (Eif {exp1=exp1, exp2=exp2, exp3=exp3}, Span.join left
            (Node.getSpan exp3))
          fun while_exp {left, exp1, exp2} =
            Node.create (Ewhile {exp1=exp1, exp2=exp2}, Span.join left
            (Node.getSpan exp2))
          fun case_exp {left, exp, matches} =
            Node.create (Ecase {exp=exp, matches=matches}, Span.join left
            (Node.getSpan (#pat (ListUtils.last matches))))
          fun fn_exp {left, matches} =
            Node.create (Efn matches, Span.join left (Node.getSpan (#pat
            (ListUtils.last matches))))
        end

        (****************)
        (* TYPE SECTION *)
        (****************)

        type tyrow = {lab: label, ty: ty}
        type tyrows = tyrow list
        type tys = ty list
        type tyseq = ty list
        type typrod = ty list
        local
          open SMLSyntax
        in
          (* Type records. *)
          val mk_tyrow = identity
          val nil_tyrows = null
          val sing_tyrows = sing
          val cons_tyrows = op ::
          val id_tyrows=  identity

          (* Type sequences. *)
          val pair_tyseq = pair
          val cons_tyseq = op ::

          (* Type atoms. *)
          fun ident_ty longid =
            Node.create (Tident longid, Node.list_span longid)
          fun tyvar_ty tyvar = Node.map (fn _ => Ttyvar tyvar) tyvar
          fun paren_ty {left, ty, right} =
            Node.map_span (Fn.const (Span.join left right)) ty
          fun record_ty {left, tyrows, right} =
            Node.create (Trecord tyrows, Span.join left right)

          (* Type constructor applications. *)
          val id_ty = identity
          fun sing_tyapp (ty, con) =
            Node.create (Tapp ([ty], con), Node.join_span ty (ListUtils.last con))
          fun mult_tyapp {left, tyseq, con} =
            Node.create (Tapp (tyseq, con), Node.join_spanl left (ListUtils.last con))

          (* Type products. *)
          val sing_typrod = sing
          val cons_typrod = op ::

          val prod_ty = fn
            [] => raise Fail "impossible: empty type product"
          | [ty] => ty
          | other => Node.create (Tprod other, Node.list_span other)
          fun arrow_ty {ty, ty'} =
            case ty of
              [] => raise Fail "impossible: empty type product"
            | [ty] =>
              Node.create (Tarrow (ty, ty'), Node.join_span ty ty')
            | tys as first::_ =>
              Node.create (Tarrow (prod_ty tys, ty'), Node.join_span first ty')
        end

        (*******************)
        (* PATTERN SECTION *)
        (*******************)

        local
          open SMLSyntax
        in

          (* Optionals. *)
          type optional_ty = ty option
          fun none_ty _ = NONE
          fun some_ty ty = SOME ty

          type optional_as_pattern = pat option
          fun none_as_pattern _ = NONE
          fun some_as_pattern pat = SOME pat

          type optional_pat = pat option
          fun none_pat _ = NONE
          fun some_pat pat = SOME pat

          (* Record patterns. *)
          datatype patrow = datatype patrow
          type patrows = patrow list
          fun lab_patrow (input as {lab, pat}) = PRlab input
          fun as_patrow (input as {id, ty, aspat}) = PRas input

          val nil_patrows = null
          fun ellipsis_patrows _ = [PRellipsis]
          val sing_patrows = sing
          val base_patrows = identity
          val cons_patrows = op ::

          (* Pattern sequences. *)
          type patseq = pat list
          val nil_patseq = null
          val pair_patseq = pair
          val cons_patseq = op ::
          val sing_patseq = sing
          val id_patseq = identity

          (* Atomic patterns. *)
          type pats = pat list
          datatype pat_parens = Bare of pat | Wrapped of pat
          fun wild_pat span = Node.create (Pwild, span)
          fun mk_node_pat constr node =
            Node.create (constr (Node.getVal node), Node.getSpan node)
          val number_pat = mk_node_pat Pnumber
          fun word_pat node =
            Node.create
              ( Pword (Symbol.fromValue (Node.getVal node))
              , Node.getSpan node
              )
          fun str_pat node=
            Node.create
              ( Pstring (Symbol.fromValue (Node.getVal node))
              , Node.getSpan node
              )
          val char_pat = mk_node_pat Pchar
          fun ident_pat id =
              Node.create (Pident {opp=false, id=id}, Node.getSpan id)
          fun ident_op_pat {opp, id} =
              Node.create (Pident {opp=true, id=id}, Node.join_spanl opp id)
          val id_pat = identity
          fun record_pat {left, patrows, right} =
            Node.create (Precord patrows, Span.join left right)
          fun constr_pat lconstr = Node.create (Pconstr {opp=false, id=lconstr}, Node.list_span lconstr)
          fun op_constr_pat {opp, constr} =
            Node.create (Pconstr {opp=true, id=constr}, Span.join opp (Node.list_span constr))
          fun tuple_pat {left, patseq, right} =
            case patseq of
              [] => Node.create (Punit, Span.join left right)
            | [x] => raise Fail "impossible - singular tuple pat"
            | _ => Node.create (Ptuple patseq, Span.join left right)
          fun or_pat {left, orpat, right} =
            case orpat of
              [] => raise Fail "impossible, empty orpat"
            | [_] => raise Fail "impossible, single orpat"
            | _ => Node.create (Por orpat, Span.join left right)
          fun list_pat {left, patseq, right} =
            Node.create (Plist patseq, Span.join left right)
          val sing_atpats = sing
          val cons_atpats = op ::
          val nil_atpats = null
          val id_atpats = identity

          (* Juxtaposed patterns. *)
          type pjuxta = pat
          type pjuxtas = pat juxta list
          val id_pjuxta = identity
          fun ref_pat span =
            Node.create (Pconstr {opp=false, id=[Node.create (ref_sym, span)]}, span)
          fun mk_pjuxta pat =
            case Node.getVal pat of
              Pident {id, opp=false} =>
                Jident (id, pat)
            | Pconstr {opp=false, id=[single]} =>
                Jident (single, pat)
            | _ => Jatom pat
          fun pair_pjuxtas (left_pat, right_pat) =
            [ mk_pjuxta left_pat, mk_pjuxta right_pat]
          fun cons_pjuxtas {pat, pjuxtas} =
            mk_pjuxta pat :: pjuxtas
            (*let
              val new_span = Node.join_span left_pat right_pat
              val new_pjuxta =
                case Node.getVal left_pat of
                  Pident {id, opp=false} =>
                    Jident (id, left_pat)
                | Pconstr {opp=false, id=[single]} =>
                      Jident (single, left_pat)
                | _ => Jatom left_pat
            in
              case Node.getVal right_pat of
                Pjuxta [juxta] => (* Should be impossible. *)
                  Node.create ( Pjuxta [new_pjuxta, juxta], new_span )
              | Pident {id, opp=false} =>
                      Node.create
                        (Pjuxta [ new_pjuxta, Jident (id, right_pat) ], new_span)
              | Pconstr {opp=false, id=[single]} =>
                  Node.create
                    (Pjuxta
                      [ new_pjuxta,
                        Jident (single,right_pat) ],
                      new_span)
              | Pjuxta juxtas =>
                  Node.create ( Pjuxta (new_pjuxta::juxtas) , new_span )
              | _ =>
                  Node.create ( Pjuxta [new_pjuxta, Jatom right_pat], new_span )
            end *)
          fun pjuxta_span (Jident (_, node)) = Node.getSpan node
            | pjuxta_span (Jatom node) = Node.getSpan node
          fun pjuxtas_pat pjuxtas =
            Node.create (Pjuxta pjuxtas, Span.join_list (List.map pjuxta_span pjuxtas))
          val pjuxta_pat = identity
          fun typed_pat (input as {pat, ty}) =
            Node.create (Ptyped input, Node.join_span pat ty)
          fun as_pat_bare {id, pat} =
              Node.create (Playered {opp=false, id=id, ty=NONE, aspat=pat},
                           Node.join_span id pat)
          fun as_pat_typed {id, ty, pat} =
              Node.create (Playered {opp=false, id=id, ty=SOME ty, aspat=pat},
                           Node.join_span id pat)
          fun check_pat pat = pat
            (* TODO: check ids dont occur mult times in a pattern *)
          (* fun as_pat {opp, id, ty, pat} =
            Node.create (Playered {opp=true, id=id, ty=ty, aspat=pat},
                         Node.join_spanl opp pat) *)
          (* fun as_pat_option {id, ty, pat} =
            let
              val span = Node.join_span id pat
            in
              Node.create (Playered {opp=false, id=id, ty=SOME ty, aspat=pat}, span)
            end *)
        end

        (***************)
        (* DEC SECTION *)
        (***************)

        (* Valbinds. *)
        type valbind = { pat : pat, exp : exp }
        type valbinds = valbind list
        val mk_valbind = identity
        val sing_valbinds = sing
        val cons_valbinds = op ::

        (* Tyvars and optionals. *)
        type tyvarseq = identifier list
        val sing_tyvars = sing
        val cons_tyvars = op ::
        fun check_tyvars tyvars = (
          all_unique id_eq tyvars;
          tyvars
        )
        val nil_tyvarseq = null
        val sing_tyvarseq = sing
        val id_tyvarseq = identity

        type optional_int = int option
        fun none_int _ = NONE
        fun some_int node = SOME (Node.getVal node)

        (* Function clauses. *)
        type fvalclause = { opp : bool, id : identifier, pats : pat list, ty : ty option, exp : exp }
        type fvalclauses = fvalclause list
        fun mk_fvalclause {left, id, atpats, ty, exp} =
          { opp=true, id=id, pats=atpats, ty=ty, exp=exp }
        fun mk_fvalclause_bare {id, atpats, ty, exp} =
          { opp=false, id=id, pats=atpats, ty=ty, exp=exp }
        fun infix_fvalclause {left_atpat, id, right_atpat, ty, exp} =
          { opp = false
          , id = id
          , pats =
            [ Node.create ( Ptuple [left_atpat, right_atpat],
                            Node.join_span left_atpat right_atpat
                          )
            ]
          , ty = ty
          , exp = exp
          }
        (* fun infix_extended_fvalclause
          {left, left_atpat, id, right_atpat, atpats, ty, exp} =
          { opp = false
          , id = id
          , pats =
            ( Node.create ( Ptuple [left_atpat, right_atpat],
                            Node.join_span left_atpat right_atpat
                          )
              :: atpats
            )
          , ty = ty
          , exp = exp
          } *)
        val sing_fvalclauses = sing
        val cons_fvalclauses = op ::

        (* Function bindings. *)
        type fvalbind = fvalclauses
        type fvalbinds = fvalclauses list
        val id_fvalbind = identity
        val sing_fvalbinds = sing
        val cons_fvalbinds = op ::
        fun check_fvalbinds fvalbinds = (
            assert_fvalbinds_valid fvalbinds;
            fvalbinds
          )

        (* Type bindings. *)
        type typbind = typbind
        type typbinds = typbind list
        val mk_typbind = identity
        val sing_typbinds = sing
        val cons_typbinds = op ::
        fun check_typbinds (typbinds : typbinds) = (
          all_unique (fn ({tycon=tycon1, ...}, {tycon=tycon2, ...}) =>
            id_eq (tycon1, tycon2)
          ) typbinds;
          typbinds
        )

        type optional_typbinds = typbinds option
        fun none_typbinds _ = NONE
        fun some_typbinds typbinds = SOME typbinds

        (* Constructor bindings. *)
        type conbind = conbind
        type conbinds = conbind list
        fun mk_conbind {opp, id, ty} = {opp=option_to_bool opp, id=id, ty=ty}
        val sing_conbinds = sing
        val cons_conbinds = op ::
        fun check_conbinds (conbinds : conbinds) = (
            all_unique (fn ({id=id1, ...}, {id=id2, ...}) =>
              id_eq (id1, id2)
            ) conbinds;
            conbinds
          )

        (* Datatype bindings. *)
        type datbind = datbind
        type datbinds = datbind list
        fun mk_datbind_bare {tycon, conbinds, deriving} =
          {tyvars=[], tycon=tycon, conbinds=conbinds, deriving = deriving}
        val mk_datbind = identity
        val sing_datbinds = sing
        val cons_datbinds = op ::

        (* Exception bindings. *)
        datatype exbind = datatype exbind
        type exbinds = exbind list
        fun mk_new_exbind {opp, id, ty} =
          Xnew { opp = option_to_bool opp
                   , id = id
                   , ty = ty }
        fun mk_repl_exbind {left_opp, left_id, right_opp, right_id} =
          Xrepl { left_opp = option_to_bool left_opp
                    , left_id = left_id
                    , right_opp = option_to_bool right_opp
                    , right_id = right_id }
        val sing_exbinds = sing
        val cons_exbinds = op ::

        (* Opened identifiers. *)
        val sing_openids = sing
        val cons_openids = op ::

        (* Declarations. *)
        local
          open SMLSyntax
        in
          fun val_dec_bare {left, recc, valbinds} = (
            assert_valbinds_valid recc valbinds;
            Node.create (Dval {recc=option_to_bool recc, tyvars=[], valbinds=valbinds},
                         Node.join_spanl left (ListUtils.map_last #pat valbinds))
            )
          fun val_dec {left, recc, tyvars, valbinds} = (
            assert_valbinds_valid recc valbinds;
            Node.create (Dval {recc=option_to_bool recc, tyvars=tyvars, valbinds=valbinds},
                         Node.join_spanl left (ListUtils.map_last #pat valbinds))
            )
          fun fun_dec {left, tyvars, fvalbinds} =
            Node.create (Dfun {tyvars=tyvars, fvalbinds=fvalbinds},
                         Node.join_spanl left ((#exp o ListUtils.last o ListUtils.last) fvalbinds))
          fun fun_dec_bare {left, fvalbinds} =
            Node.create (Dfun {tyvars=[], fvalbinds=fvalbinds},
                         Node.join_spanl left ((#exp o ListUtils.last o ListUtils.last) fvalbinds))

          fun ty_dec {left, typbinds} =
            Node.create (Dtype typbinds,
                         Node.join_spanl left ((#ty o ListUtils.last) typbinds))
          fun dat_dec {left, datbinds: datbinds, withtypee} =
            let
              val right =
                case withtypee of
                  NONE =>
                    (case (ListUtils.last o #conbinds o ListUtils.last) datbinds of
                      {ty=NONE, id, ...} => Node.getSpan id
                    | {ty=SOME ty, ...} => Node.getSpan ty)
                | SOME withtypee => Node.getSpan ((#ty o ListUtils.last) withtypee)
            in
              Node.create (Ddatdec {datbinds=datbinds, withtypee=withtypee},
                           Span.join left right)
            end
          fun datrepl_dec {left, left_tycon, right_tycon} =
            Node.create (Ddatrepl {left_tycon=left_tycon, right_tycon=right_tycon},
                         Node.join_span left_tycon (ListUtils.last right_tycon))
          fun absty_dec {left, datbinds, withtypee, dec, right} =
            Node.create (Dabstype {datbinds=datbinds, withtypee=withtypee, withh=dec},
                         Span.join left right)
          fun ex_dec {left, exbinds} =
            Node.create (Dexception exbinds,
                         Span.join
                           left
                           (((fn Xnew {ty=NONE, id, ...} => Node.getSpan id
                              | Xnew {ty=SOME ty, ...} => Node.getSpan ty
                              | Xrepl {right_id, ...} => Node.list_span right_id)
                           o ListUtils.last) exbinds))
          fun local_dec {left, left_dec, right_dec, right} =
            Node.create (Dlocal {left_dec=left_dec, right_dec=right_dec},
                         Span.join left right)
          fun open_dec {left, ids} =
            Node.create (Dopen ids,
                         Node.join_spanl left (ListUtils.last (ListUtils.last ids)))
          fun seq_dec dec = dec
          fun infix_dec {left, precedence, ids} = (
              assert_valid_precedence precedence;
              Node.create (Dinfix {precedence=precedence, ids=ids},
                           Node.join_spanl left (ListUtils.last ids))
            )
          fun infixr_dec {left, precedence, ids} = (
              assert_valid_precedence precedence;
              Node.create (Dinfixr {precedence=precedence, ids=ids},
                           Node.join_spanl left (ListUtils.last ids))
            )
          fun nonfix_dec {left, ids} =
            Node.create (Dnonfix ids,
                         Node.join_spanl left (ListUtils.last ids))

          val sing_decs = identity
          fun cons_decs (left_dec, right_dec) =
            let
              val span = Node.join_span left_dec right_dec
            in
              case Node.getVal right_dec of
                Dseq decs => Node.create (Dseq (left_dec::decs), span)
              | Dempty => Node.create (Dempty, span)
                (* I don't actually think this should happen, but whatever. *)
              | _ => Node.create (Dseq [left_dec, right_dec], span)
            end
          (* fun pair_decs (left_dec, right_dec) = cons_decseq (left_dec, right_dec) *)
          fun nil_decs () = Node.create (Dempty, Span.absurd)
            (* Keep it like this for now. *)
          val id_decs = identity
        end

        (*********************)
        (* SIGNATURE SECTION *)
        (*********************)

        (* Valdescs. *)
        type valdesc = { id : identifier, ty : ty }
        type valdescs = valdesc list
        val mk_valdesc = identity
        val sing_valdescs = sing
        val cons_valdescs = op ::

        (* Typdescs. *)
        type typdesc =
          { tyvars : identifier list
          , tycon : identifier
          , ty : ty option
          , deriving : plugins option
          }
        type typdescs = typdesc list
        fun mk_typdesc_abstract {tyvars, tycon, deriving} =
          {tyvars=tyvars, tycon=tycon, ty=NONE, deriving=deriving}
        fun mk_typdesc_concrete {tyvars, tycon, ty, deriving} =
          {tyvars=tyvars, tycon=tycon, ty=SOME ty, deriving=deriving}
        val sing_typdescs = sing
        val cons_typdescs = op ::

        (* Condescs. *)
        type condesc = {id : identifier, ty : ty option}
        type condescs = condesc list
        fun id_condesc id = {id=id, ty=NONE}
        fun ty_condesc {id, ty} = {id=id, ty=SOME ty}
        val sing_condescs = sing
        val cons_condescs = op ::

        (* Exdescs. *)
        type exdesc = {id : identifier, ty : ty option}
        type exdescs = exdesc list
        fun id_exdesc id = {id=id, ty=NONE}
        fun ty_exdesc {id, ty} = {id=id, ty=SOME ty}
        val sing_exdescs = sing
        val cons_exdescs = op ::

        (* Datdescs. *)
        type datdesc = {
          tyvars : identifier list,
          tycon : identifier,
          condescs : condescs,
          deriving : plugins option
        }
        type datdescs = datdesc list
        fun mk_datdesc_bare {tycon, condescs, deriving} =
          {tyvars=[], tycon=tycon, condescs=condescs, deriving=deriving}
        val mk_datdesc = identity
        val sing_datdescs = sing
        val cons_datdescs = op ::

        (* Strdescs. *)
        type strdesc = { id : identifier, signat : signat }
        type strdescs = strdesc list
        val mk_strdesc = identity
        val sing_strdescs = sing
        val cons_strdescs = op ::

        (* Specs. *)
        local
          open SMLSyntax
        in
          fun valdesc_spec {left, valdesc} =
            Node.create ( SPval valdesc
                        , Node.join_spanl left (#ty valdesc))
          fun typdesc_spec {left, typdesc} =
            Node.create ( SPtype typdesc
                        , Node.join_spanl left (#tycon typdesc))
          fun eqtype_spec {left, typdesc} =
            Node.create ( SPeqtype typdesc
                        , Node.join_spanl left (#tycon typdesc))
          fun datdesc_spec {left, datdesc} =
            Node.create ( SPdatdec datdesc
                        , Span.join
                            left
                            (case (ListUtils.last o #condescs) datdesc of
                              {ty=NONE, id} => Node.getSpan id
                            | {ty=SOME ty, ...} => Node.getSpan ty))
          fun datrepl_spec {left, left_tycon, right_tycon} =
            Node.create ( SPdatrepl {left_tycon=left_tycon, right_tycon=right_tycon}
                        , Node.join_spanl left (ListUtils.last right_tycon) )
          fun exdesc_spec {left, exdesc} =
            Node.create ( SPexception exdesc
                        , Span.join
                            left
                            (case exdesc of
                              {ty=NONE, id} => Node.getSpan id
                            | {ty=SOME ty, ...} => Node.getSpan ty))
          fun strdesc_spec {left, strdesc} =
            Node.create ( SPmodule strdesc
                        , Node.join_spanl left (#signat strdesc))
          fun include_spec {left, signat} =
            Node.create ( SPinclude signat, Node.join_spanl left signat )
          val id_spec = identity
          type specs = SMLSyntax.spec list
          val id_specs = identity
          val sing_specs = sing
          (* This puts in the specs backwards, so it needs to be reversed later. *)
          fun cons_specs (specs, spec) = spec :: specs
          fun empty_specs _ = []
          (* Upon seeing a sharing, assume that every single spec seen so far
           * falls under it. Then, this collapses into a single spec. *)
          fun sharing_specs (input as {specs, tycons}) =
            [ Node.create ( SPsharing {specs = List.rev specs, tycons=tycons},
                          Node.join_span
                            (List.hd specs)
                            (ListUtils.last (ListUtils.last tycons)) ) ]

          fun spec_signat {left, specs, right} =
            let
              val new_span = Span.join left right
            in
              (* Fix the absurd span here. *)
              Node.create ( Sspec (List.rev specs), new_span )
            end
          fun ident_signat id = Node.map (fn _ => Sident id) id
          fun where_signat {signat, tyvars, id, ty} =
            Node.create
              ( Swhere {signat=signat, wheretypee={tyvars=tyvars, id=id, ty=ty}}
              , Node.join_span signat ty )
        end

        (******************)
        (* MODULE SECTION *)
        (******************)

        local
          open SMLSyntax
        in
          fun struct_module {left, strdec, right} =
            Node.create (Mstruct strdec, Span.join left right)
          fun ident_module id =
            Node.create (Mident id, Node.list_span id)
          fun transparent_module {module, signat} =
            Node.create
              ( Mseal {module=module, opacity=Transparent, signat=signat}
              , Node.join_span module signat )
          fun opaque_module {module, signat} =
            Node.create
              ( Mseal {module=module, opacity=Opaque, signat=signat}
              , Node.join_span module signat )
          fun app_module {id, module, right} =
            Node.create ( Mapp {functorr=id, arg=Normal_app module}
                        , Node.join_spanr id right )
          fun app_module_sugar {id, strdec, right} =
            Node.create ( Mapp {functorr=id, arg=Sugar_app strdec}
                        , Node.join_spanr id right )
          fun let_module {left, dec, module, right} =
            Node.create ( Mlet {dec=dec, module=module}
                        , Span.join left right )

          type strbind = {
              id : identifier,
              seal : { opacity : opacity, signat : signat } option,
              module : module
            }
          type strbinds = strbind list
          fun mk_strbind_bare {id, module} =
            {id=id, seal=NONE, module=module}
          fun mk_strbind_seal_trans {id, signat, module} =
            {id=id, seal=SOME {opacity=Transparent, signat=signat}, module=module}
          fun mk_strbind_seal_opaque {id, signat, module} =
            {id=id, seal=SOME {opacity=Opaque, signat=signat}, module=module}

          val sing_strbinds = sing
          val cons_strbinds = op ::

          type strdec = strdec
          fun dec_strdec dec = Node.map (fn _ => DMdec dec) dec
          val id_strdec = identity
          fun cons_strdec {left_strdec, right_strdec} =
            let
              val new_span = Node.join_span left_strdec right_strdec
            in
              case (Node.getVal left_strdec, Node.getVal right_strdec) of
                (DMdec left_dec, DMdec right_dec) =>
                  let
                    val new_span = Node.join_span left_strdec right_strdec
                  in
                    (case Node.getVal right_dec of
                      Dempty => left_strdec (* Pretty sure this is impossible. *)
                    | Dseq decs =>
                        Node.create (
                          DMdec ( Node.create ( Dseq (left_dec :: decs), new_span) ),
                          new_span
                        )
                    | other =>
                        Node.create (
                          DMdec ( Node.create ( Dseq [left_dec, right_dec], new_span ) ),
                          new_span
                        ))
                  end
              | (_, DMseq (new_right_strdec::rest)) =>
                  (case Node.getVal new_right_strdec of
                    DMdec _ =>
                      cons_strdec
                        {left_strdec =
                          cons_strdec {left_strdec = left_strdec,
                                       right_strdec = new_right_strdec},
                         right_strdec =
                          Node.create ( DMseq rest, Node.list_span rest ) }
                  | _ =>
                      Node.create (DMseq (left_strdec::new_right_strdec::rest),
                                   new_span))
              | other => Node.create (DMseq [left_strdec, right_strdec], new_span)
            end
          fun nil_strdec () = Node.create ( DMempty, Span.absurd)
            (* Keep the span absurd. *)
          fun structure_strdec {left, strbinds} =
            Node.create ( DMstruct strbinds
                        , Node.join_spanl left ((#module o ListUtils.last) strbinds ) )
          fun local_strdec {left, left_dec, right_dec, right} =
            Node.create ( DMlocal {left_dec=left_dec, right_dec=right_dec}
                        , Span.join left right )
          fun seq_strdec strdecs = Node.create ( DMseq strdecs, Node.list_span strdecs )
          val pair_strdecs = pair

          type sigbind = {id : identifier, signat : signat}
          type sigbinds = sigbind list
          val mk_sigbind = identity
          val sing_sigbinds = sing
          val cons_sigbinds = op ::

          type sigdec = sigbinds Node.t
          fun mk_sigdec {left, sigbinds: sigbinds} =
            Node.create (sigbinds, Node.join_spanl left ((#signat o ListUtils.last) sigbinds))

          datatype funarg = datatype funarg
          val normal_funarg = Normal
          val sugar_funarg = Sugar

          type funbind = {
            id : identifier,
            funarg : funarg,
            seal : { signat : signat, opacity : opacity } option,
            body : module
          } Node.t
          type funbinds = funbind list
          type fundec = funbinds Node.t
          fun bare_funbind (input as {id, funarg, body}) =
            Node.create
              ( { id = id
                , funarg = funarg
                , seal = NONE
                , body = body }
              , Node.join_span id body
              )
          fun transparent_funbind (input as {id, funarg, signat, body}) =
            Node.create
              ( { id = id
                , funarg = funarg
                , seal = SOME { signat = signat, opacity = Transparent }
                , body = body }
              , Node.join_span id body
              )
          fun opaque_funbind (input as {id, funarg, signat, body}) =
            Node.create
              ( { id = id
                , funarg = funarg
                , seal = SOME { signat = signat, opacity = Opaque }
                , body = body }
              , Node.join_span id body
              )
          val sing_funbinds = sing
          val cons_funbinds = op ::

          fun mk_fundec {left, funbinds} =
            Node.create (funbinds, Node.join_spanl left (ListUtils.last funbinds))

          datatype topdec = datatype topdec
          type topdecs = topdec list
          val top_strdec = Strdec
          val top_sigdec = Sigdec
          val top_fundec = Fundec

          val empty_program = null
          val cons_topdecs = op ::
        end

        exception Error of Token.token StreamStreamable.t
        fun error x = Error x
      end

    (* Sidestepping the NJ extension so it can parse itself. *)
    structure Input =
      struct
        structure Streamable = StreamStreamable
        structure Arg = Arg
      end

    structure ParseMain =
      ParserFun (Input)

    fun parse cs =
      let
        val (ast, stream) = ParseMain.parse (Lexer.lex cs)
      in
        Either.INR (ast, Stream.toList stream)
      end
        handle Arg.Error x => Either.INL (List.map Token.token_to_string
        (Stream.toList x))

    fun parse_string s = parse (Stream.fromList (String.explode s))

    fun parse_file s =
      let
        val instream = TextIO.openIn s
        val input = TextIO.inputAll instream
      in
        parse_string input
      end

    fun parse_file_transformed s =
      case parse_file s of
        Either.INR (ast, right) => Either.INR (Transform.transform ast, right)
      | other => other

    fun parse_file_to_string s =
      case parse_file s of
        Either.INL strs => Error.err (Error.ParseError (s, strs))
      | Either.INR (ast, _) => PrettyPrintAst.pretty ast true

    fun parse_file_transformed_to_string s =
      case parse_file s of
        Either.INL strs => Error.err (Error.ParseError (s, strs))
      | Either.INR (ast, _) => PrettyPrintAst.pretty (Transform.transform ast) true
  end
