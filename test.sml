
structure Test =
  struct

    infix |>
    fun x |> f = f x

    (*
    structure TC = TerminalColors

    fun text color s = TC.foreground color ^ s ^ TC.reset
     *)

    val text = Fn.id

    fun strip s =
      List.foldr
        (fn (c, acc) =>
          case c of
            #" " => acc
          | #"\n" => acc
          | _ => c::acc
        )
        []
        (String.explode s)
      |> String.implode

    fun check_eq l =
      ( List.appi
          (fn (idx, (s, s')) =>
            ( ( if s = s' then (
                  text (* TC.green *) ("Test " ^ Int.toString idx ^ " passed!\n\n")
                )
                else (
                    text (* TC.red *) ("Test " ^ Int.toString idx ^ " failed!\n")
                  ^ text (* TC.red *) "Expected: " ^ text (* TC.white *) s' ^ "\n"
                  ^ text (* TC.red *) "Got: " ^ text (* TC.white *) s ^ "\n\n"
                )
              )
              |> print
            ; ()
            )
          )
          l
      ; l
      )

    datatype simple1 = A | B [.deriving show]

    datatype const =
        Int of int
      | Real of real
      | Char of char
      | String of string
      | Bool of bool [.deriving show]

    datatype 'a poly = Poly of 'a [.deriving show]

    datatype mono =
        Mono1 of int list
      | Mono2 of int option
      | Mono3 of int poly [.deriving show]

    fun run_tests l =
      [ ( [.show: simple1] A, "A" )
      , ( [.show: simple1] B, "B" )
      , ( show_simple1 A, "A" )
      , ( simple1_show A, "A" )
      , ( [.show: const] (Int 15150)        , "Int (15150)")
      , ( [.show: const] (Real 15150.0)     , "Real (15150.0)")
      , ( [.show: const] (Char #"v")        , "Char (#\"v\")")
      , ( [.show: const] (String "onefifty"), "String (\"onefifty\")")
      , ( [.show: const] (Bool true)        , "Bool (true)")
      , ( [.show: mono] (Mono1 [1, 5, 0])   , "Mono1 ([1, 5, 0])")
      , ( [.show: mono] (Mono2 (SOME 150))  , "Mono2 (SOME (150))")
      , ( [.show: mono] (Mono3 (Poly 150))  , "Mono3 (Poly (150))")
      , ( [.show: int poly] (Poly 150)      , "Poly (150)")
      , ( [.show: bool * int] (true, 150)   , "(true, 150)")
      , ( [.show: bool * (int * string)] (true, (150, "stan"))
        , "(true, (150, \"stan\"))"
        )
      , ( [.show: (bool * int) * string] ((true, 150), "stan")
        , "((true, 150), \"stan\")"
        )
      , ( [.show: {x : bool, y : int}] {x = true, y = 150}
        , "{x = true, y = 150}"
        )
      , ( [.show: {x : bool, w : {y : int, z : string}}] {x = true, w = {y = 150, z = "stan"}}
        , "{x = true, w = {y = 150, z = \"stan\"}}"
        )
      , ( [.show: {w : {x : bool, y : int}, z : string}] {w = {x = true, y = 150}, z = "stan"}
        , "{w = {x = true, y = 150}, z = \"stan\"}"
        )
      , ( [.show: (int * int) option] (SOME (1, 50)), "SOME ((1, 50))" )
      , ( [.show: int -> int] (fn x => x), "<fn>" )
      , ( [.show: (int -> string) option] (SOME (fn x => x)), "SOME (<fn>)" )
      ]
      |> check_eq
  end
