
structure Foo =
  struct
    type t =
        One
      | Two [@@deriving show]

    val x = show_t One
    val y = [%show: t] One
  end

val x = [%show: Foo.t] One
val y = Foo.show_t One
