
signature SCOPED_DICT =
  sig
    type key
    type 'a dict
    type 'a t

    val empty : 'a t

    exception Outer
    val pop_scope : 'a t -> 'a dict * 'a t
    (* For things like local <dec> in <dec> end, when we want to throw away the
     * second-least-recent scope frame. *)
    val pop_penultimate : 'a t -> 'a dict * 'a t
    val new_scope : 'a t -> 'a t
    val num_scopes : 'a t -> int

    val insert : 'a t -> key -> 'a -> 'a t
    val remove : 'a t -> key -> 'a t

    val contains : 'a t -> key -> bool

    exception NotFound
    val lookup : 'a t -> key -> 'a
  end

functor ScopedDict (KeyDict : DICT) :>
  SCOPED_DICT where type key = KeyDict.key
              where type 'a dict = 'a KeyDict.dict =
  struct
    structure KeyDict = KeyDict

    type key = KeyDict.key
    type 'a dict = 'a KeyDict.dict
    type 'a t = 'a dict * 'a dict list

    val empty = (KeyDict.empty, [])

    exception Outer
    fun pop_scope (dict, rest) = (dict, (List.hd rest, List.tl rest)) handle _ => raise Outer
    fun pop_penultimate (dict, []) = raise Fail "cannot pop: only 1 scope"
      | pop_penultimate (dict, hd::tl) = (hd, (dict, tl))
    fun new_scope (dict, rest) = (KeyDict.empty, dict::rest)
    fun num_scopes (_, rest) = 1 + List.length rest

    fun insert (dict, rest) k v = (KeyDict.insert dict k v, rest)
    fun remove (dict, rest) k = (KeyDict.remove dict k, rest)

    fun contains (dict, rest) k =
      KeyDict.member dict k
      orelse
        (case rest of
          [] => false
        | x::xs => contains (x, xs) k)

    exception NotFound
    fun lookup (dict, rest) k =
      case (KeyDict.find dict k, rest) of
        (SOME v, _) => v
      | (NONE, []) => raise NotFound
      | (NONE, d::ds) => lookup (d, ds) k
  end

structure StrDict = RedBlackDict(structure Key = StringOrdered)
structure ScopeDict = ScopedDict(StrDict)
