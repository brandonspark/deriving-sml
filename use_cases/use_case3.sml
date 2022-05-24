
structure Foo :
  sig
    datatype t = One | Two [.deriving show]
  end =
  struct
    datatype t =
        One
      | Two [.deriving show]

    val x = show_t One
    val y = [.show: t] One
  end

val x = [.show: Foo.t] Foo.One
val y = Foo.show_t Foo.One

structure Foo2 :
  sig
    type t [.deriving show]

    val init : t
  end =
  struct
    type t = int [.deriving show]

    val init = 5
  end

val x = [.show: Foo2.t] Foo2.init
