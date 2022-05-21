
structure OptionMonad :
  sig
    include MONAD

    val value : 'a m -> 'a -> 'a
    val map2 : ('a -> 'b -> 'c) -> 'a m -> 'b m -> 'c m
    val value_map : 'a m -> 'b -> ('a -> 'b) -> 'b

    val is_some : 'a m -> bool
    val is_none : 'a m -> bool
  end =
  struct
    type 'a m = 'a option

    val return = SOME
    fun bind m f =
      case m of
        SOME v => f v
      | NONE => NONE
    fun >>= (m, f) = bind m f

    fun map m f = bind m (fn input => SOME (f input))
    fun >>| (m, f) = map m f

    fun join input = bind input Fn.id
    fun all ms =
      List.foldl
        (fn (x, acc) =>
          bind x (fn inner =>
          bind acc (fn inner_list =>
          SOME (inner::inner_list))))
        (SOME [])
        ms

    fun value m default =
      case m of
        SOME v => v
      | NONE => default
    fun map2 f left right =
      bind left (fn inner_left =>
      bind right (fn inner_right =>
      SOME (f inner_left inner_right)))
    fun value_map m default f =
      case m of
        SOME v => f v
      | NONE => default

    fun is_some m =
      case m of
        SOME _ => true
      | _ => false
    fun is_none m =
      case m of
        NONE => true
      | _ => false
  end
