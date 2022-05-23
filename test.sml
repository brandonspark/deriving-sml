structure Foo =
  struct
    datatype t = One | Two
    fun show_t t5 = (case t5 of One => "One" | Two => "Two")
    and t_show x = (show_t x)
    val x = (show_t One)
    val y = ((fn t3 => (t_show t3)) One)
  end
val x = ((fn t4 => (Foo.t_show t4)) Foo.One)
val y = (Foo.show_t Foo.One)
