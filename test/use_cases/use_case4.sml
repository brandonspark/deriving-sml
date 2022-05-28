
datatype const =
    Int of int
  | Real of real
  | Bool of bool
  | Char of char
  | String of string [.deriving show]

val a = [.show: const] (Int 5)
val b = [.show: const] (Real 0.0)
val c = [.show: const] (Bool true)
val d = [.show: const] (Char #"a")
val e = [.show: const] (String "hi")
