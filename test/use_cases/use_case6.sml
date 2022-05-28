
type t = int * string [.deriving show]

(* should produce
 * fun t_show (t1, t2) =
 *   "(" ^ Int.toString t1 ^ ", " ^ Int.toString t2 ^ ")"
 *)

(* type t = t * t [.deriving show] should fail *)

datatype t = Base | Recur of int * t [.deriving show] (* should work *)

(* should produce
 *
 * fun t_show t =
 *  case t of
 *    Base =>
 *  | Recur (t1, t2) => "Recur" ^
 *)
