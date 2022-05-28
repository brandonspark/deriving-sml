
(* NOTE: from last time I tried this, you get fucked from signatures
 * if you're gonna try and keep track of values and types in scope, you gotta
 * know what modules really ascribe to
 * this means you need to save every existing signature's contents
 * also, where types aren't very nice for the AST, they're kinda annoying.
 * so, this is kinda just a lot of annoyance
 *)
signature MODULAR_DICT =
  sig
    type key
    type 'a t


    val empty : 'a t
    (* something like a
     * { main : Dict.t
     * , saved :
     *)
    val new_scope : key option * t -> t

    val exit_scope : t -> t

    val add_entry : key -> 'a -> 'a t -> 'a t

    val contains : key list ->
  end

functor ModularDict (KeyDict : DICT) :>
  SCOPED_DICT where type key = KeyDict.key
                and type 'a dict = 'a KeyDict.dict =
  struct
    structure ScopeDict = ScopedDict (KeyDict)
    type key = KeyDict.key
    type 'a dict = 'a KeyDict.dict

    (* A dictionary for mapping ids to 'a
     * and a dictionary for mapping ids to other modules
     *)

    datatype 'a pre = Module of 'a dict * 'a pre dict

    type 'a t =
      { namespace : (identifier * signat) option list
      , scopes : 'a pre ScopeDict.t (* "current" modular scope *)
      , modules : 'a pre           (* "existing" modular scopes *)
      }

    val empty =
      { namespace = [NONE]
      , scopes = ScopeDict.empty
      , modules = Module (KeyDict.empty, KeyDict.empty)
      }

    fun new_scope (id, {namespace, scopes, modules}) =
      { namespace = id :: namespace
      , scopes = ScopeDict.new_scope scopes
      , modules = modules
      }

    fun exit_scope {namespace, scopes, modules = Module (main, modules)} =
      case namespace of
        [] => raise Fail "exiting final scope"
      | outer::rest =>
          let
            val (cur_mod, tl_pres) = ScopeDict.pop_scope scopes
          in
            { namespace = rest
            , scopes = tl_scopes
            , modules =
              Module
                ( case outer of
                    NONE => KeyDict.union hd_pre main snd
                  | SOME mod_name =>
                      (* We're exiting a module, and all the stuff in it.
                       * The main namespace is not changed, but we shadow that
                       * module name (if it exists)
                      ( main
                      , KeyDict.insertMerge
                          modules
                          mod_name
                          cur_mod
                          (fn _ => cur_mod)
                      )

          end






  end
