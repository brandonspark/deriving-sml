
signature COMMON =
  sig
    val apply : 'a -> ('a -> 'b) -> 'b
    val |> : 'a * ('a -> 'b) -> 'b
  end

structure Common : COMMON =
  struct
    fun apply x f = f x
    fun |> (x, f) = f x
  end
