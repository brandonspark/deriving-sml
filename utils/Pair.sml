
signature PAIR =
  sig
    val &&& : ('a -> 'b) * ('a -> 'c) -> 'a -> 'b * 'c
    val *** : ('a -> 'b) * ('c -> 'd) -> 'a * 'c -> 'b * 'd
    val map_fst : ('a -> 'c) -> 'a * 'b -> 'c * 'b
    val map_snd : ('b -> 'c) -> 'a * 'b -> 'a * 'c
  end

structure Pair : PAIR =
  struct
    fun &&& (f, g) = fn x => (f x, g x)
    fun *** (f, g) = fn (x, y) => (f x, g y)

    fun map_fst f (x, y) = (f x, y)
    fun map_snd f (x, y) = (x, f y)
  end
