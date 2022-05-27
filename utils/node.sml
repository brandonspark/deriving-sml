
signature NODE =
  sig
    type 'a t

    val create : 'a * Span.span -> 'a t
    val create2 : 'a -> Span.span -> 'a t
    val create_absurd : 'a -> 'a t
    val expose : 'a t -> {value: 'a, span: Span.span}
    val nest : 'a t -> 'a t t

    val getVal : 'a t -> 'a
    val getSpan : 'a t -> Span.span

    val sing : 'a t -> 'a list t
    val cons : 'a t * 'a list t -> 'a list t

    val join_span : 'a t -> 'b t -> Span.span
    val join_spanl : Span.span -> 'a t -> Span.span
    val join_spanr : 'a t -> Span.span -> Span.span

    val map : ('a -> 'b) -> 'a t -> 'b t
    val map_span : (Span.span -> Span.span) -> 'a t -> 'a t
    val list_span : 'a t list -> Span.span

    val location_insensitive_eq : ('a * 'a -> bool) -> 'a t * 'a t -> bool
  end
structure Node : NODE =
  struct
    type 'a t = {
      value : 'a,
      span : Span.span
    }

    fun create (value, span) = {value=value, span=span}
    fun create2 value span = create (value, span)
    fun create_absurd value = create (value, Span.absurd)
    fun expose {value, span} = create (value, span)
    fun nest (input as {value, span}) = {value=input, span=span}

    fun getVal {value, span} = value
    fun getSpan {value, span} = span

    fun sing {value, span} = {value=[value], span=span}
    fun cons ({value=sing_val, span=sing_span}, {value=cons_val,
    span=cons_span}) =
      {value=sing_val::cons_val, span=Span.join sing_span cons_span}

    fun join_span ({span=left_span, ...}: 'a t) ({span=right_span, ...}: 'b t) =
      Span.join left_span right_span
    fun join_spanl span node = Span.join span (getSpan node)
    fun join_spanr node span = Span.join (getSpan node) span

    fun map f {value, span} = {value=f value, span=span}
    fun map_span f {value, span} = {value=value, span=f span}
    val list_span = fn
      [] => raise Fail "finding list span of empty list"
    | other => List.foldr (fn (node, span) => Span.join (getSpan
    node) span) (getSpan (List.nth (other, List.length other - 1))) other

    fun location_insensitive_eq p (left, right) = p (getVal left, getVal right)
  end
