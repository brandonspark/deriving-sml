
type t =
    One
  | Two [@@deriving show]

val x = [%show: t] One
val y = show_t One

