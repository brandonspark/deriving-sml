
datatype 'a t = Foo | Bar of 'a

(* ideally, should generate
 * pp_t : ('a -> string) -> 'a t -> string
 *)

val x = [.show: int t] (Bar 5)

(* should desugar to `pp_t show_int` *)
