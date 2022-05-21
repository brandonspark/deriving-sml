
signature MONAD =
  sig
    type 'a m

    val return : 'a -> 'a m
    val bind : 'a m -> ('a -> 'b m) -> 'b m
    val >>= : 'a m * ('a -> 'b m) -> 'b m

    val map : 'a m -> ('a -> 'b) -> 'b m
    val >>| : 'a m * ('a -> 'b) -> 'b m

    val join : 'a m m -> 'a m
    val all : 'a m list -> 'a list m
  end
