
signature CONTEXT =
  sig
    (* A context comprises of two things, a general and modular context.
     *
     * The general context is scoped to have layers that are pushed and popped
     * as the scope requires. It will be used to "simulate" a module or
     * signature context, and at the end, any "published" modular bindings are
     * transferred to the modular context.
     *
     * Modular contexts do not go out of scope unless they are constructed from
     * signatures. They can be shadowed, however. *)

    type t
    datatype assoc = Left | Right
    val init : t

    val pop_scope : t -> t
    (* For things like local <dec> in <dec> end, when we want to throw away the
     * second-least-recent scope frame. *)
    val pop_penultimate : t -> t
    val new_scope : t -> t

    val add_infix : string -> {assoc : assoc, precedence : int} -> t -> t
    val remove_infix : string -> t -> t
    (*
    val add_ty : string -> t -> t
    val add_exp : string -> t -> t

    val bind_scope_to_path : string -> t -> t
    *)
    val lookup_infix : string -> t -> {assoc : assoc, precedence : int} option

    (*
    val exists_ty : path -> t -> bool
    val exists_exp : path -> t -> bool
     *)
  end

structure Context :> CONTEXT =
  struct
    (* can have module names mapping to contexts *)
    (* there is the "empty map" as well, for any top level contexts *)
    (* descoping does nothing to module contexts, only to empty context *)
    (* because of calling convention, there should always be only one top level
     * context when declaring a module *)
    (* descoping module contexts can only happen when theyre being shadowed, or
      * when you are exiting a signature *)
    (*
    type env = {
      infix_env : { assoc : assoc, precedence : int } ScopeDict.t,
      ty_env    : unit ScopeDict.t,
      exp_env   : unit ScopeDict.t
    }

    type path_env = {
      mod_infix : { assoc : assoc, precedence : int } ScopeDict.dict,
      mod_ty    : unit ScopeDict.dict,
      mod_exp   : unit ScopeDict.dict
    }

    type t = env * path_env ScopeDict.dict (* map module names to environments *)

    fun pop_scope ({infix_env, ty_env, exp_env}, mod_ctx) =
      let
        val (_, new_infixes) = ScopeDict.pop_scope infix_env
        val (_, new_tys) = ScopeDict.pop_scope ty_env
        val (_, new_exps) = ScopeDict.pop_scope exp_env
      in
        ( { infix_env = new_infixes
          , ty_env = new_tys
          , exp_env = new_exps }
        , mod_ctx )
      end
    fun new_scope ({infix_env, ty_env, exp_env}, mod_ctx) =
      ( { infix_env = ScopeDict.new_scope infix_env
        , ty_env = ScopeDict.new_scope ty_env
        , exp_env = ScopeDict.new_scope exp_env },
        mod_ctx )

    fun update_infixes new_infixes ({ty_env, exp_env, ...} : env) =
        { infix_env = new_infixes
        , ty_env = ty_env
        , exp_env = exp_env }
    fun update_tys new_tys ({infix_env, exp_env, ...} : env) =
        { infix_env = infix_env
        , ty_env = new_tys
        , exp_env = exp_env }
    fun update_exps new_exps ({infix_env, ty_env, ...} : env) =
        { infix_env = infix_env
        , ty_env = ty_env
        , exp_env = new_exps }

    fun add_infix name info (gen_ctx as {infix_env, ...} : env, mod_ctx) =
      ( update_infixes
          (ScopeDict.insert infix_env name info)
          gen_ctx
      , mod_ctx )
    fun add_ty name (gen_ctx as {ty_env, ...} : env, mod_ctx) =
      ( update_tys
          (ScopeDict.insert ty_env name ())
          gen_ctx
      , mod_ctx )
    fun add_exp name (gen_ctx as {exp_env, ...} : env, mod_ctx) =
      ( update_exps
          (ScopeDict.insert exp_env name ())
          gen_ctx
      , mod_ctx )

    fun bind_scope_to_path path ({infix_env, ty_env, exp_env}, mod_ctx) =
      let
        val (infix_scope, infix_env) = ScopeDict.pop_scope infix_env
        val (ty_scope, ty_env) = ScopeDict.pop_scope ty_env
        val (exp_scope, exp_env) = ScopeDict.pop_scope exp_env
      in
        ( { infix_env = infix_env
          , ty_env = ty_env
          , exp_env = exp_env }
        , StrDict.insert mod_ctx path
            { mod_infix = infix_scope
            , mod_ty = ty_scope
            , mod_exp = exp_scope } )
      end *)

    datatype assoc = Left | Right

    type infixes = { assoc : assoc, precedence : int }

    type t = infixes ScopeDict.t

    fun pop_scope ctx = (#2 (ScopeDict.pop_scope ctx))
    fun pop_penultimate ctx = (#2 (ScopeDict.pop_penultimate ctx))
    fun new_scope ctx = ScopeDict.new_scope ctx

    fun add_infix name {assoc, precedence} ctx =
      ScopeDict.insert ctx name {assoc=assoc, precedence=precedence}
    fun remove_infix name ctx =
      ScopeDict.remove ctx name

    local
      open Common
      infix |>
    in
      val basis = [ ("div", Left, 7),
                  ("mod", Left, 7),
                  ("*", Left, 7),
                  ("/", Left, 7),
                  ("+", Left, 6),
                  ("-", Left, 6),
                  ("<", Left, 4),
                  (">", Left, 4),
                  ("<=", Left, 4),
                  (">=", Left, 4),

                  ("::", Right, 5),
                  ("=", Left, 4),
                  (":=", Left, 3) ]
      val init =
        basis
        |> List.map (fn (name, assoc, precedence) => (name, {assoc=assoc, precedence=precedence}))
        |> List.foldl
             (fn ((name, status), ctx) => add_infix name status ctx)
             ScopeDict.empty
    end

    fun lookup_infix name ctx =
      SOME (ScopeDict.lookup ctx name) handle ScopeDict.NotFound => NONE

  end
