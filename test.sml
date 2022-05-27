
structure Test :
  sig
    val run_tests : unit -> unit
  end =
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

    fun check_eq toString l =
      ( List.appi
          (fn (idx, (s, s')) =>
            ( ( if s = s' then (
                  text (* TC.green *) ("Test " ^ Int.toString idx ^ " passed!\n\n")
                )
                else (
                    text (* TC.red *) ("Test " ^ Int.toString idx ^ " failed!\n")
                  ^ text (* TC.red *) "Expected: " ^ text (* TC.white *) (toString s') ^ "\n"
                  ^ text (* TC.red *) "Got: " ^ text (* TC.white *) (toString s) ^ "\n\n"
                )
              )
              |> print
            ; ()
            )
          )
          l
      ; ()
      )

    datatype simple1 = A | B [.deriving show, eq, compare, map]

    datatype const =
        Int of int
      | Real of real
      | Char of char
      | String of string (* uncommenting the below should cause an error *)
      | Bool of bool [.deriving show, compare, map (*, eq *)]

    datatype 'a poly = Poly of 'a [.deriving show, eq, cmp, map]

    datatype mono =
        Mono1 of int list
      | Mono2 of int option
      | Mono3 of int poly [.deriving show, eq, cmp, map]

    fun run_show_tests l =
      [ ( [.show: simple1] A                , "A" )
      , ( [.show: simple1] B                , "B" )
      , ( show_simple1 A                    , "A" )
      , ( simple1_show A                    , "A" )
      , ( [.show: const] (Int 15150)        , "Int (15150)")
      , ( [.show: const] (Real 15150.0)     , "Real (15150.0)")
      , ( [.show: const] (Char #"v")        , "Char (#\"v\")")
      , ( [.show: const] (String "onefifty"), "String (\"onefifty\")")
      , ( [.show: const] (Bool true)        , "Bool (true)")
      , ( [.show: mono] (Mono1 [1, 5, 0])   , "Mono1 ([1, 5, 0])")
      , ( [.show: mono] (Mono2 (SOME 150))  , "Mono2 (SOME (150))")
      , ( [.show: mono] (Mono3 (Poly 150))  , "Mono3 (Poly (150))")
      , ( [.show: order] LESS               , "LESS")
      , ( [.show: order] EQUAL              , "EQUAL")
      , ( [.show: order] GREATER            , "GREATER")
      , ( [.show: int poly] (Poly 150)      , "Poly (150)")
      , ( show_poly Int.toString (Poly 150) , "Poly (150)")
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
      , ( [.show: int -> int] (fn x => x)     , "<fn>" )
      , ( [.show: (int -> string) option] (SOME (fn x => x)), "SOME (<fn>)" )
      , ( [.show: 'a] Int.toString 150, "150" )
      , ( [.show: 'a * 'b] Int.toString Fn.id (150, "> 122"), "(150, > 122)")

      , ( [.show: 'b * 'a] Fn.id Int.toString (150, "> 122"), "(150, > 122)" )
      , ( [.show: 'a option] Int.toString (SOME 150), "SOME (150)" )
      , ( [.show: bool * 'a] Int.toString (true, 150), "(true, 150)" )
      ]
      |> check_eq Fn.id

    datatype const_no_real =
        Int2 of int
      | Char2 of char
      | String2 of string
      | Bool2 of bool [.deriving eq, map]

    fun run_eq_tests l =
      [ ( [.eq: simple1] (A, A), true )
      , ( [.eq: simple1] (A, B), false )
      , ( [.eq: simple1] (B, B), true )
      , ( eq_simple1 (A, A), true )
      , ( simple1_eq (A, B), false )
      , ( [.eq: const_no_real] (Int2 15150, Int2 15150), true)
      , ( [.eq: const_no_real] (Int2 15150, Int2 15122), false)
      , ( [.eq: const_no_real] (Char2 #"v", Char2 #"v"), true)
      , ( [.eq: const_no_real] (Char2 #"v", Char2 #"x"), false)
      , ( [.eq: const_no_real] (String2 "150", String2 "150"), true)
      , ( [.eq: const_no_real] (String2 "150", String2 "122"), false)
      , ( [.eq: const_no_real] (Bool2 true, Bool2 true), true)
      , ( [.eq: const_no_real] (Bool2 true, Bool2 false), false)
      , ( [.eq: mono] (Mono1 [1, 5, 0], Mono1 [1, 5, 0]), true)
      , ( [.eq: mono] (Mono1 [1, 5, 0], Mono1 [1, 2, 2]), false)
      , ( [.eq: mono] (Mono2 (SOME 150), Mono2 (SOME 150)), true)
      , ( [.eq: mono] (Mono2 (SOME 150), Mono2 (SOME 122)), false)
      , ( [.eq: mono] (Mono3 (Poly 150), Mono3 (Poly 150)), true)
      , ( [.eq: mono] (Mono3 (Poly 150), Mono3 (Poly 122)), false)
      , ( [.eq: order] (LESS, LESS), true)
      , ( [.eq: order] (EQUAL, EQUAL), true)
      , ( [.eq: order] (GREATER, GREATER), true)
      , ( [.eq: order] (LESS, EQUAL), false)
      , ( [.eq: order] (EQUAL, GREATER), false)
      , ( [.eq: int poly] (Poly 150, Poly 150), true)
      , ( [.eq: int poly] (Poly 150, Poly 122), false)
      , ( poly_eq (op=) (Poly 150, Poly 150), true)
      , ( poly_eq (op=) (Poly 150, Poly 122), false)
      , ( [.eq: int * bool] ((150, true), (150, true)), true)
      , ( [.eq: int * bool] ((150, true), (122, true)), false)
      , ( [.eq: int * bool] ((150, true), (150, false)), false)
      , ( [.eq: bool * (int * string)]
          ((true, (150, "stan")), (true, (150, "stan")))
        , true
        )
      , ( [.eq: bool * (int * string)]
          ((true, (150, "stan")), (false, (150, "stan")))
        , false
        )
      , ( [.eq: bool * (int * string)]
          ((true, (150, "stan")), (true, (122, "stan")))
        , false
        )
      , ( [.eq: bool * (int * string)]
          ((true, (150, "stan")), (true, (150, "bob")))
        , false
        )
      , ( [.eq: (bool * int) * string]
          (((true, 150), "stan"), ((true, 150), "stan"))
        , true
        )
      , ( [.eq: (bool * int) * string]
          (((true, 150), "stan"), ((false, 150), "stan"))
        , false
        )
      , ( [.eq: (bool * int) * string]
          (((true, 150), "stan"), ((true, 122), "stan"))
        , false
        )
      , ( [.eq: (bool * int) * string]
          (((true, 150), "stan"), ((true, 150), "bob"))
        , false
        )
      , ( [.eq: {x : int, y : bool}] ({x = 150, y = true}, {x = 150, y = true}), true)
      , ( [.eq: {x : int, y : bool}] ({x = 150, y = true}, {x = 122, y = false}), false)
      , ( [.eq: {x : int, y : bool}] ({x = 150, y = true}, {x = 150, y = false}), false)
      , ( [.eq: {x : bool, w : {y : int, z : string}}]
          ( {x = true, w = {y = 150, z = "stan"}}
          , {x = true, w = {y = 150, z = "stan"}}
          )
        , true
        )
      , ( [.eq: {x : bool, w : {y : int, z : string}}]
          ( {x = true, w = {y = 150, z = "stan"}}
          , {x = false, w = {y = 150, z = "stan"}}
          )
        , false
        )
      , ( [.eq: {x : bool, w : {y : int, z : string}}]
          ( {x = true, w = {y = 150, z = "stan"}}
          , {x = true, w = {y = 122, z = "stan"}}
          )
        , false
        )
      , ( [.eq: {x : bool, w : {y : int, z : string}}]
          ( {x = true, w = {y = 150, z = "stan"}}
          , {x = true, w = {y = 150, z = "bob"}}
          )
        , false
        )
      , ( [.eq: {w : {x : bool, y : int}, z : string}]
          ( {w = {x = true, y = 150}, z = "stan"}
          , {w = {x = true, y = 150}, z = "stan"}
          )
        , true
        )
      , ( [.eq: {w : {x : bool, y : int}, z : string}]
          ( {w = {x = true, y = 150}, z = "stan"}
          , {w = {x = false, y = 150}, z = "stan"}
          )
        , false
        )
      , ( [.eq: {w : {x : bool, y : int}, z : string}]
          ( {w = {x = true, y = 150}, z = "stan"}
          , {w = {x = true, y = 122}, z = "stan"}
          )
        , false
        )
      , ( [.eq: {w : {x : bool, y : int}, z : string}]
          ( {w = {x = true, y = 150}, z = "stan"}
          , {w = {x = true, y = 150}, z = "bob"}
          )
        , false
        )
      (* uncommenting this should cause an error
      , ( [.eq: int -> string] ((fn _ => ""), (fn _ => "")), true)
       *)
      , ( [.eq: (int * int) option] (SOME (150, 312), SOME (150, 312)), true )
      , ( [.eq: (int * int) option] (SOME (150, 312), SOME (312, 150)), false )
      , ( [.eq: (int * int) option] (SOME (150, 312), SOME (150, 122)), false )
      , ( [.eq: (int * int) option] (SOME (122, 312), SOME (150, 312)), false )
      , ( [.eq: 'a] (fn (x, y) => x = y + 1) (150, 149) , true )
      , ( [.eq: 'a * 'b]
            (fn (x, y) => x = y + 1)
            (fn (x, y) => x + 1 = y)
            ((150, 150), (149, 151))
        , true
        )
      , ( [.eq: 'b * 'a]
            (fn (x, y) => x + 1 = y)
            (fn (x, y) => x = y + 1)
            ((150, 150), (149, 151))
        , true
        )
      , ( [.eq: bool * 'a] (fn (x, y) => x = y + 1) ((true, 150), (true, 149)), true)
      ]
      |> check_eq Bool.toString

    fun run_cmp_tests l =
      [ ( [.compare: simple1] (A, A), EQUAL )
      , ( [.cmp: simple1] (A, A), EQUAL )
      , ( [.compare: simple1] (A, B), LESS )
      , ( [.compare: simple1] (B, A), GREATER )
      , ( [.compare: simple1] (B, B), EQUAL )
      , ( compare_simple1 (A, A), EQUAL )
      , ( simple1_compare (A, B), LESS )
      , ( cmp_simple1 (A, A), EQUAL )
      , ( simple1_cmp (A, B), LESS )
      , ( [.compare: const] (Int 15150, Int 15150), EQUAL)
      , ( [.compare: const] (Int 15150, Int 15122), GREATER)
      , ( [.compare: const] (Int 15122, Int 15150), LESS)
      , ( [.compare: const] (Char #"v", Char #"v"), EQUAL)
      , ( [.compare: const] (Char #"v", Char #"x"), LESS)
      , ( [.compare: const] (Char #"x", Char #"v"), GREATER)
      , ( [.compare: const] (String "150", String "150"), EQUAL)
      , ( [.compare: const] (String "150", String "122"), GREATER)
      , ( [.compare: const] (String "122", String "150"), LESS)
      , ( [.compare: const] (Bool true, Bool true), EQUAL)
      , ( [.compare: const] (Bool false, Bool false), EQUAL)
      , ( [.compare: const] (Bool true, Bool false), GREATER)
      , ( [.compare: const] (Bool false, Bool true), LESS)
      , ( [.compare: const] (Real 15150.0, Real 15150.0), EQUAL)
      , ( [.compare: const] (Real 15150.0, Real 15122.0), GREATER)
      , ( [.compare: const] (Real 15122.0, Real 15150.0), LESS)
      , ( [.compare: mono] (Mono1 [1, 5, 0], Mono1 [1, 5, 0]), EQUAL)
      , ( [.compare: mono] (Mono1 [1, 5, 0], Mono1 [1, 2, 2]), GREATER)
      , ( [.compare: mono] (Mono1 [1, 2, 2], Mono1 [1, 5, 0]), LESS)
      , ( [.compare: mono] (Mono1 [], Mono1 [1]), LESS)
      , ( [.compare: mono] (Mono1 [1], Mono1 []), GREATER)
      , ( [.compare: mono] (Mono2 (SOME 150), Mono2 (SOME 150)), EQUAL)
      , ( [.compare: mono] (Mono2 (SOME 150), Mono2 (SOME 122)), GREATER)
      , ( [.compare: mono] (Mono2 (SOME 122), Mono2 (SOME 150)), LESS)
      , ( [.compare: mono] (Mono3 (Poly 150), Mono3 (Poly 150)), EQUAL)
      , ( [.compare: mono] (Mono3 (Poly 150), Mono3 (Poly 122)), GREATER)
      , ( [.compare: mono] (Mono3 (Poly 122), Mono3 (Poly 150)), LESS)
      , ( [.compare: order] (LESS, LESS), EQUAL)
      , ( [.compare: order] (EQUAL, EQUAL), EQUAL)
      , ( [.compare: order] (GREATER, GREATER), EQUAL)
      , ( [.compare: order] (LESS, EQUAL), LESS)
      , ( [.compare: order] (EQUAL, LESS), GREATER)
      , ( [.compare: order] (GREATER, EQUAL), GREATER)
      , ( [.compare: order] (GREATER, LESS), GREATER)
      , ( [.compare: int poly] (Poly 150, Poly 150), EQUAL)
      , ( [.compare: int poly] (Poly 150, Poly 122), GREATER)
      , ( [.compare: int poly] (Poly 122, Poly 150), LESS)
      , ( poly_compare Int.compare (Poly 150, Poly 150), EQUAL)
      , ( poly_compare Int.compare (Poly 150, Poly 122), GREATER)
      , ( poly_compare Int.compare (Poly 122, Poly 150), LESS)
      , ( [.compare: int * bool] ((150, true), (150, true)), EQUAL)
      , ( [.compare: int * bool] ((150, true), (122, true)), GREATER)
      , ( [.compare: int * bool] ((150, false), (122, true)), GREATER)
      , ( [.compare: int * bool] ((150, true), (122, false)), GREATER)
      , ( [.compare: bool * (int * string)]
          ((true, (150, "stan")), (true, (150, "stan")))
        , EQUAL
        )
      , ( [.compare: bool * (int * string)]
          ((true, (150, "stan")), (false, (150, "stan")))
        , GREATER
        )
      , ( [.compare: bool * (int * string)]
          ((false, (150, "stan")), (true, (150, "stan")))
        , LESS
        )
      , ( [.compare: bool * (int * string)]
          ((true, (150, "stan")), (true, (122, "stan")))
        , GREATER
        )
      , ( [.compare: bool * (int * string)]
          ((false, (150, "stan")), (true, (122, "stan")))
        , LESS
        )
      , ( [.compare: bool * (int * string)]
          ((true, (150, "stan")), (true, (150, "bob")))
        , GREATER
        )
      , ( [.compare: bool * (int * string)]
          ((true, (122, "stan")), (true, (150, "bob")))
        , LESS
        )
      , ( [.compare: (bool * int) * string]
          (((true, 150), "stan"), ((true, 150), "stan"))
        , EQUAL
        )
      , ( [.compare: (bool * int) * string]
          (((true, 150), "stan"), ((false, 150), "stan"))
        , GREATER
        )
      , ( [.compare: (bool * int) * string]
          (((true, 122), "stan"), ((true, 150), "stan"))
        , LESS
        )
      , ( [.compare: (bool * int) * string]
          (((true, 122), "stan"), ((false, 150), "stan"))
        , GREATER
        )
      , ( [.compare: (bool * int) * string]
          (((true, 150), "stan"), ((true, 122), "stan"))
        , GREATER
        )
      , ( [.compare: (bool * int) * string]
          (((true, 150), "stan"), ((true, 150), "bob"))
        , GREATER
        )
      , ( [.compare: {x : int, y : bool}] ({x = 150, y = true}, {x = 150, y = true}), EQUAL)
      , ( [.compare: {x : int, y : bool}] ({x = 150, y = true}, {x = 122, y = false}), GREATER)
      , ( [.compare: {x : int, y : bool}] ({x = 150, y = true}, {x = 150, y = false}), GREATER)
      , ( [.compare: {x : bool, w : {y : int, z : string}}]
          ( {x = true, w = {y = 150, z = "stan"}}
          , {x = true, w = {y = 150, z = "stan"}}
          )
        , EQUAL
        )
      , ( [.compare: {x : bool, w : {y : int, z : string}}]
          ( {x = true, w = {y = 150, z = "stan"}}
          , {x = false, w = {y = 150, z = "stan"}}
          )
        , GREATER
        )
      , ( [.compare: {x : bool, w : {y : int, z : string}}]
          ( {x = true, w = {y = 150, z = "stan"}}
          , {x = true, w = {y = 122, z = "stan"}}
          )
        , GREATER
        )
      , ( [.compare: {x : bool, w : {y : int, z : string}}]
          ( {x = false, w = {y = 150, z = "stan"}}
          , {x = true, w = {y = 122, z = "stan"}}
          )
        , LESS
        )
      , ( [.compare: {x : bool, w : {y : int, z : string}}]
          ( {x = true, w = {y = 150, z = "stan"}}
          , {x = true, w = {y = 150, z = "bob"}}
          )
        , GREATER
        )
      , ( [.compare: {x : bool, w : {y : int, z : string}}]
          ( {x = true, w = {y = 122, z = "stan"}}
          , {x = true, w = {y = 150, z = "bob"}}
          )
        , LESS
        )
      , ( [.compare: {w : {x : bool, y : int}, z : string}]
          ( {w = {x = true, y = 150}, z = "stan"}
          , {w = {x = true, y = 150}, z = "stan"}
          )
        , EQUAL
        )
      , ( [.compare: {w : {x : bool, y : int}, z : string}]
          ( {w = {x = true, y = 150}, z = "stan"}
          , {w = {x = false, y = 150}, z = "stan"}
          )
        , GREATER
        )
      , ( [.compare: {w : {x : bool, y : int}, z : string}]
          ( {w = {x = true, y = 150}, z = "stan"}
          , {w = {x = true, y = 122}, z = "stan"}
          )
        , GREATER
        )
      , ( [.compare: {w : {x : bool, y : int}, z : string}]
          ( {w = {x = true, y = 150}, z = "stan"}
          , {w = {x = true, y = 150}, z = "bob"}
          )
        , GREATER
        )
      (* uncommenting this should cause an error
      , ( [.compare: int -> string] ((fn _ => ""), (fn _ => "")), true)
       *)
      , ( [.compare: (int * int) option] (SOME (150, 312), SOME (150, 312)), EQUAL )
      , ( [.compare: (int * int) option] (SOME (150, 312), SOME (312, 150)), LESS )
      , ( [.compare: (int * int) option] (SOME (150, 312), SOME (150, 122)), GREATER )
      , ( [.compare: (int * int) option] (SOME (122, 312), SOME (150, 122)), LESS )
      , ( [.compare: (int * int) option] (SOME (122, 312), SOME (150, 312)), LESS )
      , ( [.compare: (int * int) option] (NONE, SOME (150, 312)), LESS )
      , ( [.compare: (int * int) option] (SOME (150, 312), NONE), GREATER )
      , ( [.compare: 'a] Int.compare (150, 151), LESS )
      , ( [.compare: 'a * 'b]
            Int.compare
            String.compare
            ((150, "a"), (150, "b"))
        , LESS
        )
      , ( [.compare: 'a * 'b]
            Int.compare
            String.compare
            ((151, "a"), (150, "b"))
        , GREATER
        )
      , ( [.compare: 'b * 'a]
            String.compare
            Int.compare
            ((150, "a"), (150, "b"))
        , LESS
        )
      , ( [.compare: 'b * 'a]
            String.compare
            Int.compare
            ((151, "a"), (150, "b"))
        , GREATER
        )
      , ( [.compare: bool * 'a] Int.compare ((true, 150), (true, 150)), EQUAL)
      , ( [.compare: bool * 'a] Int.compare ((true, 150), (true, 151)), LESS)
      , ( [.compare: bool * 'a] Int.compare ((true, 150), (false, 151)), GREATER)
      ]
      |> check_eq [.show: order]

    datatype ('a, 'b) poly2 =
        Poly1 of int * 'a list * 'b option
      | Poly2 of int * { x : 'a option, y : 'b list } [.deriving map, eq]

    fun run_map_tests l =
      [ ( map_simple1 A, A ) |> [.eq: simple1]
      , ( simple1_map A, A ) |> [.eq: simple1]
      , ( map_const_no_real (Int2 15150), Int2 15150 ) |> [.eq: const_no_real]
      , ( map_const_no_real (Char2 #"v"), Char2 #"v" )  |> [.eq: const_no_real]
      , ( map_const_no_real (String2 "150"), String2 "150") |> [.eq: const_no_real]
      , ( map_const_no_real (Bool2 true), Bool2 true) |> [.eq: const_no_real]
      , ( mono_map (Mono1 [1, 5, 0]), Mono1 [1, 5, 0]) |> [.eq: mono]
      , ( mono_map (Mono2 (SOME 150)), Mono2 (SOME 150)) |> [.eq: mono]
      , ( mono_map (Mono3 (Poly 150)), Mono3 (Poly 150)) |> [.eq: mono]
      (* this should cause an error
      , ( [.map: order] LESS, LESS) |> [.eq: order]
       *)
      , ( poly_map (fn x => x + 162) (Poly 150), Poly 312) |> [.eq: int poly]
      , ( map_poly (fn x => x + 162) (Poly 150), Poly 312) |> [.eq: int poly]
      , ( poly_map (fn x => x + 162) (Poly 150), Poly 312) |> [.eq: int poly]
      , ( poly_map (fn x => x + 162) (Poly 150), Poly 312) |> [.eq: int poly]
      (* ought not to work
      , ( [.map: int * int] (15, 150), EQUAL)
      , ( [.map: {x : int, y : bool}] {x = 150, y = true}, EQUAL)
       *)
      , ( poly2_map not (fn s => s ^ " there") (Poly1 (150, [true], SOME "hi"))
        , (Poly1 (150, [false], SOME "hi there"))
        ) |> [.eq: (bool, string) poly2]
      , ( poly2_map not (fn s => s ^ " there") (Poly1 (150, [true], NONE))
        , (Poly1 (150, [false], NONE))
        ) |> [.eq: (bool, string) poly2]
      , ( poly2_map not (fn s => s ^ " there")
            (Poly2 (150, { x = SOME true, y = ["who", "why"] }))
        , (Poly2 (150, { x = SOME false, y = ["who there", "why there"]}))
        ) |> [.eq: (bool, string) poly2]
      , ( poly2_map not (fn s => s ^ " there")
            (Poly2 (150, { x = NONE, y = ["who", "why"] }))
        , (Poly2 (150, { x = NONE, y = ["who there", "why there"]}))
        ) |> [.eq: (bool, string) poly2]
      , ( [.map : 'a] (fn x => x + 1) 150, 151) |> [.eq: int]
      , ( [.map : 'a * 'b] not Int.toString (true, 150), (false, "150")) |> [.eq: int * string]
      , ( [.map: 'b * 'a] Int.toString not (true, 150), (false, "150")) |> [.eq: int * string]
      , ( [.map: int * 'a] not (150, true), (150, false)) |> [.eq: int * bool]
      , ( [.map: 'a list list] Int.toString [[], [150]], [[], ["150"]]) |> [.eq: int list list]
      ]
      |> List.map (fn true => (true, true) | false => (false, true))
      |> check_eq [.show: bool]

    fun run_tests () =
      ( run_show_tests ()
      ; run_eq_tests ()
      ; run_cmp_tests ()
      ; run_map_tests ()
      )

  end
