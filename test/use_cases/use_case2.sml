
structure Foo =
  struct
    datatype t =
        One
      | Two [.deriving show]

    val x = show_t One
    val y = [.show: t] One
  end

val x = [.show: Foo.t] Foo.One
val y = Foo.show_t Foo.One
