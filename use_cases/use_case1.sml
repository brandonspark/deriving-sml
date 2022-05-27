
datatype t =
    One
  | Two [.deriving show, eq]

val y = show_t One

datatype t2 =
    Three of int option
  | Four of string list [.deriving show]

val z = show_t2
