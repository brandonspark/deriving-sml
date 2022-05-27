
structure PreSMLSyntax =
  struct
    type symbol = Symbol.symbol
    type span = Span.span
    type identifier = symbol Node.t
    type longid = identifier list

    datatype 'a juxta =
        Jident of identifier * 'a
      | Jatom of 'a

    (****************************)
    (*        DERIVING          *)
    (****************************)

    (* Name of the setting, and its value *)
    type setting = identifier * identifier
    type settings = setting list

    (* Plugin name, with all of its settings *)
    type plugin = identifier * settings
    type plugins = plugin list

    (****************************)
    (*         TYPES            *)
    (****************************)

    datatype ty_ =
        Tident of longid
      | Ttyvar of identifier
      | Tapp of ty list * longid
      | Tprod of ty list
      | Tarrow of ty * ty
      | Trecord of {lab: identifier, ty: ty} list

    withtype ty = ty_ Node.t

    (****************************)
    (*        PATTERNS          *)
    (****************************)

    datatype patrow =
        PRellipsis
      | PRlab of {
          lab : identifier,
          pat : pat
        }
      | PRas of {
          id : identifier,
          ty : ty option,
          aspat : pat option
        }

    and pat_ =
      (* scons *)
        Pnumber of int
      | Pword of symbol
      | Pstring of symbol
      | Pchar of char

      (* atpats *)
      | Pwild
      | Pident of {
          opp : bool,
          id : identifier
        }
      | Pconstr of { opp : bool, id : longid }
      | Precord of patrow list
      | Punit
      | Ptuple of pat list
      | Plist of pat list
      | Por of pat list

      (* pats *)
      | Papp of {
          id : longid,
          atpat : pat
        }
      | Ptyped of {
          pat : pat,
          ty : ty
        }
      | Playered of {
          opp : bool,
          id : identifier,
          ty : ty option,
          aspat : pat
        }

      | Pjuxta of pat juxta list

    withtype pat = pat_ Node.t

    (****************************)
    (*       EXPRESSIONS        *)
    (****************************)

    datatype exbind =
        Xnew of {
          opp : bool,
          id : identifier,
          ty : ty option
        }
      | Xrepl of {
          left_opp : bool,
          left_id : identifier,
          right_opp : bool,
          right_id : longid
        }

    datatype number =
        Int of int
      | Word of string
      | Real of real

    type conbind = {
        opp : bool,
        id : identifier,
        ty : ty option
      }
    type typbind = {
        tyvars : identifier list,
        tycon : identifier,
        ty : ty,
        deriving : plugins option
      }
    type datbind = {
        tyvars : identifier list,
        tycon : identifier,
        conbinds : conbind list,
        deriving : plugins option
      }

    datatype exp_ =
        Enumber of number (* int, real, hex, ... *)
      | Estring of symbol
      | Echar of char
      | Erecord of {
          lab : identifier,
          exp : exp
        } list
      | Eselect of identifier
      | Eunit
      | Eident of {
          opp : bool,
          id : longid
        }
      | Econstr of {
          opp : bool,
          id : longid
        }
      | Etuple of exp list
      | Elist of exp list
      | Eseq of exp list
      | Elet of {
          dec : dec,
          exps : exp list
        }
      | Eapp of {
          left : exp,
          right : exp
        }
      | Etyped of {
          exp : exp,
          ty : ty
        }
      | Eandalso of {
          left : exp,
          right : exp
        }
      | Eorelse of {
          left : exp,
          right : exp
        }
      | Ehandle of {
          exp : exp,
          matches : { pat : pat, exp : exp } list
        }
      | Eraise of exp
      | Eif of {
          exp1 : exp,
          exp2 : exp,
          exp3 : exp
        }
      | Ewhile of {
          exp1 : exp,
          exp2 : exp
        }
      | Ecase of {
          exp : exp,
          matches : { pat : pat, exp : exp } list
        }
      | Efn of { pat : pat, exp : exp } list

      | Ejuxta of exp juxta list

    and dec_ =
        Dval of {
          recc : bool,
          tyvars : identifier list,
          valbinds : { pat : pat, exp : exp } list
        }
      | Dfun of { (* need to do something about infixed function names *)
          tyvars : identifier list,
          fvalbinds : {
            opp : bool,
            id: identifier,
            pats : pat list,
            ty : ty option,
            exp : exp
          } list list
        }
      | Dtype of typbind list
      | Ddatdec of {
          datbinds : datbind list,
          withtypee : typbind list option
        }
      | Ddatrepl of {
          left_tycon : identifier,
          right_tycon : longid
          (* TODO: datrepl plugins *)
        }
      | Dabstype of {
          datbinds : datbind list,
          withtypee : typbind list option,
          withh : dec
          (* TODO?: abstype plugins *)
        }
      | Dexception of exbind list
      | Dlocal of {
          left_dec : dec,
          right_dec : dec
        }
      | Dopen of longid list
      | Dempty
      | Dseq of dec list (* should not be nested *)
      | Dinfix of {
          precedence : int option,
          ids : identifier list
        }
      | Dinfixr of {
          precedence : int option,
          ids : identifier list
        }
      | Dnonfix of identifier list

    withtype dec = dec_ Node.t
    and exp = exp_ Node.t

    (****************************)
    (*         MODULES          *)
    (****************************)

    type condesc = {
        id : identifier,
        ty : ty option
      }

    type typdesc = {
        tyvars : identifier list,
        tycon : identifier,
        ty : ty option,
        deriving : plugins option
      }

    datatype strdec_ =
        DMdec of dec
      | DMstruct of {
          id : identifier,
          seal : { opacity : opacity, signat : signat } option,
          module : module
        } list
      | DMlocal of {
          left_dec : strdec,
          right_dec : strdec
        }
      | DMseq of strdec list
      | DMempty

    and module_ =
        Mident of longid
      | Mstruct of strdec
      | Mseal of {
          module : module,
          opacity : opacity,
          signat : signat
        }
      | Mapp of {
          functorr : identifier,
          module : module
        }
      | Mlet of {
          dec : strdec,
          module : module
        }

    and signat_ =
        Sspec of spec list
      | Sident of identifier
      | Swhere of {
          signat : signat,
          wheretypee : {
            tyvars : identifier list,
            id : longid,
            ty : ty
          }
        }

    and spec_ =
        SPval of {
          id : identifier,
          ty : ty
        }
      | SPtype of typdesc
      | SPeqtype of typdesc
      | SPdatdec of {
          tyvars : identifier list,
          tycon : identifier,
          condescs : condesc list,
          deriving : plugins option
        }
      | SPdatrepl of {
          left_tycon : identifier,
          right_tycon : longid
          (* TODO: datrepl plugins *)
        }
      | SPexception of {
          id : identifier,
          ty : ty option
        }
      | SPmodule of {
          id : identifier,
          signat : signat
        }
      | SPinclude of signat
      | SPsharing of {
          specs : spec list,
          tycons : longid list (* longtycon1 = .. = longtycon_n *)
        }

    and opacity =
        Transparent
      | Opaque

    withtype module = module_ Node.t
    and strdec = strdec_ Node.t
    and signat = signat_ Node.t
    and spec = spec_ Node.t

    type sigbinds = {id : identifier, signat : signat} list
    type sigdec = sigbinds Node.t

    (****************************)
    (*        FUNCTORS          *)
    (****************************)

    type funbind = {
        id : identifier,
        arg_id : identifier,
        signat : signat,
        seal : { signat : signat, opacity : opacity } option,
        body : module
      }
    type funbinds = funbind Node.t list
    type fundec = funbinds Node.t

    (****************************)
    (*         TOPDECS          *)
    (****************************)

    datatype topdec =
        Strdec of strdec
      | Sigdec of sigdec
      | Fundec of fundec

    type ast = topdec list
  end

signature SMLSYNTAX =
  sig
    type symbol = PreSMLSyntax.symbol
    type span = PreSMLSyntax.span
    type identifier = PreSMLSyntax.identifier
    type longid = PreSMLSyntax.longid

    datatype juxta = datatype PreSMLSyntax.juxta

    val mk_id : string -> identifier
    val id_to_string : identifier -> string
    val id_eq : identifier * identifier -> bool
    val longid_to_string : longid -> string
    val juxta_span : ('a list -> 'b) -> 'a juxta list -> 'b
    val map_sym : symbol -> (string -> string) -> symbol

    (* DERIVING *)
    type setting = identifier * identifier
    type settings = setting list

    type plugin = identifier * settings
    type plugins = plugin list

    (* TYPES *)

    datatype ty_ = datatype PreSMLSyntax.ty_
    type ty = PreSMLSyntax.ty

    (* PATS *)

    datatype patrow = datatype PreSMLSyntax.patrow
    datatype pat_ = datatype PreSMLSyntax.pat_
    type pat = PreSMLSyntax.pat

    (* EXPS *)

    datatype exbind = datatype PreSMLSyntax.exbind
    datatype number = datatype PreSMLSyntax.number

    type conbind = PreSMLSyntax.conbind
    type typbind = PreSMLSyntax.typbind
    type datbind = PreSMLSyntax.datbind

    datatype exp_ = datatype PreSMLSyntax.exp_
    datatype dec_ = datatype PreSMLSyntax.dec_
    type dec = PreSMLSyntax.dec
    type exp = PreSMLSyntax.exp

    (* MODULES *)

    type condesc = PreSMLSyntax.condesc
    type typdesc = PreSMLSyntax.typdesc

    datatype strdec_ = datatype PreSMLSyntax.strdec_
    datatype module_ = datatype PreSMLSyntax.module_
    datatype signat_ = datatype PreSMLSyntax.signat_
    datatype spec_ = datatype PreSMLSyntax.spec_
    datatype opacity = datatype PreSMLSyntax.opacity

    type module = PreSMLSyntax.module
    type strdec = PreSMLSyntax.strdec
    type signat = PreSMLSyntax.signat
    type spec = PreSMLSyntax.spec

    type sigbinds = PreSMLSyntax.sigbinds
    type sigdec = PreSMLSyntax.sigdec

    (* FUNCTORS *)

    type funbind = PreSMLSyntax.funbind
    type funbinds = PreSMLSyntax.funbinds
    type fundec = PreSMLSyntax.fundec

    (* TOPDECS *)

    datatype topdec = datatype PreSMLSyntax.topdec

    type ast = PreSMLSyntax.ast
  end

structure SMLSyntax : SMLSYNTAX =
  struct
    type symbol = PreSMLSyntax.symbol
    type span = PreSMLSyntax.span
    type identifier = PreSMLSyntax.identifier
    type longid = PreSMLSyntax.longid

    datatype juxta = datatype PreSMLSyntax.juxta

    fun mk_id s = Node.create (Symbol.fromValue s, Span.absurd)
    fun id_to_string id = Symbol.toValue (Node.getVal id)
    fun id_eq (id1, id2) =
      Node.location_insensitive_eq Symbol.eq (id1, id2)
    fun longid_to_string id =
      List.foldr
        op^
        ""
        (List.map (Symbol.toValue o Node.getVal) id)
    fun juxta_span list_span juxtas =
      list_span (List.map (fn Jident (_, a) => a | Jatom a => a) juxtas)
    fun map_sym sym f =
      Symbol.fromValue (f (Symbol.toValue sym))

    (* DERIVING *)
    type setting = PreSMLSyntax.setting
    type settings = PreSMLSyntax.settings

    type plugin = PreSMLSyntax.plugin
    type plugins = PreSMLSyntax.plugins

    (* TYPES *)

    datatype ty_ = datatype PreSMLSyntax.ty_
    type ty = PreSMLSyntax.ty

    (* PATS *)

    datatype patrow = datatype PreSMLSyntax.patrow
    datatype pat_ = datatype PreSMLSyntax.pat_
    type pat = PreSMLSyntax.pat

    (* EXPS *)

    datatype exbind = datatype PreSMLSyntax.exbind
    datatype number = datatype PreSMLSyntax.number

    type conbind = PreSMLSyntax.conbind
    type typbind = PreSMLSyntax.typbind
    type datbind = PreSMLSyntax.datbind

    datatype exp_ = datatype PreSMLSyntax.exp_
    datatype dec_ = datatype PreSMLSyntax.dec_
    type dec = PreSMLSyntax.dec
    type exp = PreSMLSyntax.exp

    (* MODULES *)

    type condesc = PreSMLSyntax.condesc
    type typdesc = PreSMLSyntax.typdesc

    datatype strdec_ = datatype PreSMLSyntax.strdec_
    datatype module_ = datatype PreSMLSyntax.module_
    datatype signat_ = datatype PreSMLSyntax.signat_
    datatype spec_ = datatype PreSMLSyntax.spec_
    datatype opacity = datatype PreSMLSyntax.opacity

    type module = PreSMLSyntax.module
    type strdec = PreSMLSyntax.strdec
    type signat = PreSMLSyntax.signat
    type spec = PreSMLSyntax.spec

    type sigbinds = PreSMLSyntax.sigbinds
    type sigdec = PreSMLSyntax.sigdec

    (* FUNCTORS *)

    type funbind = PreSMLSyntax.funbind
    type funbinds = PreSMLSyntax.funbinds
    type fundec = PreSMLSyntax.fundec

    (* TOPDECS *)
    datatype topdec = datatype PreSMLSyntax.topdec

    type ast = PreSMLSyntax.ast
  end
