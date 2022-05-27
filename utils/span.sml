
signature SPAN =
  sig
    type pos = int
    type span = pos * pos

    val join : span -> span -> span
    val join_list : span list -> span

    val absurd : span
    val is_absurd : span -> bool
  end

structure Span :> SPAN =
  struct
    type pos = int
    type span = pos * pos

    fun join (l, _) (_, r) = (l, r)
    val join_list = fn
      [] => raise Fail "joining empty list of spans"
    | other => List.foldr (Fn.uncurry join) (List.nth (other, List.length
    other - 1)) other

    val absurd = (~1, ~1)
    fun is_absurd span = span = absurd
  end
