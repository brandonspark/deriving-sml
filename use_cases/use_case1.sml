
datatype t =
    One
  | Two [.deriving show]

val y = show_t One
