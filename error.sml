
exception Warn of (int * unit SMLofNJ.Cont.cont)

(* This fact doesn't like the numbers 1, 3, 7, and 11, and thus cries whenever
 * those occur.
 *)
fun biased_fact n =
  case n of
    0 => 1
  | _ =>
      ( case n of
          ( 1 | 3 | 7 | 11 ) =>
            SMLofNJ.Cont.callcc
              (fn cont => raise Warn (n, cont))
        | _ => ()
      ; n * biased_fact (n - 1)
      )

fun run () =
  biased_fact 12 handle Warn (n, cont) =>
    ( print ("biased fact is unhappy because it got " ^ (Int.toString n) ^ " :( \n")
    ; SMLofNJ.Cont.throw cont ()
    )
