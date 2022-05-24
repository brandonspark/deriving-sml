structure Test =
  struct
    infix |>
    fun x |> f = (f ( x ))
    val text = Fn.id
    fun strip s =
        (op|> ( ((((List.foldr ( (fn (c, acc) =>
                                       (case c of
                                          #" " => acc
                                        | #"\n" => acc
                                        | _ =>
                                            (op:: ( (c, acc) )))) )) ( [] )) ( (String.explode ( s )) )), 
                 String.implode) ))
    fun check_eq l =
        (((List.appi ( (fn (idx, (s, s')) =>
                             ((op|> ( ((if
                                        (op= ( (s, s') ))
                                        then
                                        (text ( (op^ ( ((op^ ( ("Test ", 
                                                                (Int.toString ( idx ))) )), 
                                                        " passed!\n\n") )) ))
                                        else
                                        (op^ ( ((op^ ( ((op^ ( ((op^ ( ((op^ ( ((op^ ( ((text ( (op^ ( ((op^ ( ("Test ", 
                                                                                                                (Int.toString ( idx ))) )), 
                                                                                                        " failed!\n") )) )), 
                                                                                        (text ( "Expected: " ))) )), 
                                                                                (text ( s' ))) )), 
                                                                        "\n") )), 
                                                                (text ( "Got: " ))) )), 
                                                        (text ( s ))) )), 
                                                "\n\n") ))), 
                                       print) )); 
                              ())) )) ( l )); 
         l)
    datatype simple1 = A | B (* [.deriving show] *)
    fun show_simple1 t45 = (case t45 of A => "A" | B => "B")
    and simple1_show x = (show_simple1 ( x ))
    datatype const =
      Int of int
    | Real of real
    | Char of char
    | String of string
    | Bool of bool
    (* [.deriving show] *)
    fun show_const t51 =
        (case t51 of
           (Int t46) =>
             (op^ ( ((op^ ( ("Int (", (Int.toString ( t46 ))) )), 
                     ")") ))
         | (Real t47) =>
             (op^ ( ((op^ ( ("Real (", (Real.toString ( t47 ))) )), 
                     ")") ))
         | (Char t48) =>
             (op^ ( ((op^ ( ("Char (", 
                             (op^ ( ((op^ ( ("#\"", 
                                             (Char.toString ( t48 ))) )), 
                                     "\"") ))) )), 
                     ")") ))
         | (String t49) =>
             (op^ ( ((op^ ( ("String (", 
                             (op^ ( ((op^ ( ("\"", t49) )), "\"") ))) )), 
                     ")") ))
         | (Bool t50) =>
             (op^ ( ((op^ ( ("Bool (", 
                             (if t50 then "true" else "false")) )), 
                     ")") )))
    and const_show x = (show_const ( x ))
    datatype 'a poly = Poly of 'a  (* [.deriving show] *)
    fun show_poly fn0 t53 =
        (case t53 of
           (Poly t52) =>
             (op^ ( ((op^ ( ("Poly (", (fn0 ( t52 ))) )), ")") ))
         )
    and poly_show x = (show_poly ( x ))
    datatype mono =
      Mono1 of int list
    | Mono2 of int option
    | Mono3 of int poly
    (* [.deriving show] *)
    fun show_mono t60 =
        (case t60 of
           (Mono1 t54) =>
             (op^ ( ((op^ ( ("Mono1 (", 
                             (op^ ( ((op^ ( ("[", 
                                             ((String.concatWith ( ", " )) ( ((List.map ( (fn t55 =>
                                                                                                (Int.toString ( t55 ))) )) ( t54 )) ))) )), 
                                     "]") ))) )), 
                     ")") ))
         | (Mono2 t56) =>
             (op^ ( ((op^ ( ("Mono2 (", 
                             (case t56 of
                                NONE => "NONE"
                              | (SOME x) =>
                                  (op^ ( ((op^ ( ("SOME (", 
                                                  ((fn t57 =>
                                                         (Int.toString ( t57 ))) ( x ))) )), 
                                          ")") )))) )), 
                     ")") ))
         | (Mono3 t58) =>
             (op^ ( ((op^ ( ("Mono3 (", 
                             ((poly_show ( (fn t59 =>
                                                 (Int.toString ( t59 ))) )) ( t58 ))) )), 
                     ")") )))
    and mono_show x = (show_mono ( x ))
    fun run_tests l =
        (op|> ( ([(((fn t0 => (simple1_show ( t0 ))) ( A )), "A"), 
                  (((fn t1 => (simple1_show ( t1 ))) ( B )), "B"), 
                  ((show_simple1 ( A )), "A"), 
                  ((simple1_show ( A )), "A"), 
                  (((fn t2 => (const_show ( t2 ))) ( (Int ( 15150 )) )), 
                   "Int (15150)"), 
                  (((fn t3 => (const_show ( t3 ))) ( (Real ( 15150.0 )) )), 
                   "Real (15150.0)"), 
                  (((fn t4 => (const_show ( t4 ))) ( (Char ( #"v" )) )), 
                   "Char (#\"v\")"), 
                  (((fn t5 => (const_show ( t5 ))) ( (String ( "onefifty" )) )), 
                   "String (\"onefifty\")"), 
                  (((fn t6 => (const_show ( t6 ))) ( (Bool ( true )) )), 
                   "Bool (true)"), 
                  (((fn t7 => (mono_show ( t7 ))) ( (Mono1 ( [1, 
                                                              5, 
                                                              0] )) )), 
                   "Mono1 ([1, 5, 0])"), 
                  (((fn t8 => (mono_show ( t8 ))) ( (Mono2 ( (SOME ( 150 )) )) )), 
                   "Mono2 (SOME (150))"), 
                  (((fn t9 => (mono_show ( t9 ))) ( (Mono3 ( (Poly ( 150 )) )) )), 
                   "Mono3 (Poly (150))"), 
                  (((fn t10 =>
                          ((poly_show ( (fn t11 =>
                                              (Int.toString ( t11 ))) )) ( t10 ))) ( (Poly ( 150 )) )), 
                   "Poly (150)"), 
                  (((fn (t13, t14) =>
                          (op^ ( ((op^ ( ((op^ ( ("(", 
                                                  (if
                                                   t13
                                                   then
                                                   "true"
                                                   else "false")) )), 
                                          (op^ ( (", ", 
                                                  (Int.toString ( t14 ))) ))) )), 
                                  ")") ))) ( (true, 150) )), 
                   "(true, 150)"), 
                  (((fn (t16, (t18, t19)) =>
                          (op^ ( ((op^ ( ((op^ ( ("(", 
                                                  (if
                                                   t16
                                                   then
                                                   "true"
                                                   else "false")) )), 
                                          (op^ ( (", ", 
                                                  (op^ ( ((op^ ( ((op^ ( ("(", 
                                                                          (Int.toString ( t18 ))) )), 
                                                                  (op^ ( (", ", 
                                                                          (op^ ( ((op^ ( ("\"", 
                                                                                          t19) )), 
                                                                                  "\"") ))) ))) )), 
                                                          ")") ))) ))) )), 
                                  ")") ))) ( (true, 
                                              (150, "stan")) )), 
                   "(true, (150, \"stan\"))"), 
                  (((fn ((t22, t23), t24) =>
                          (op^ ( ((op^ ( ((op^ ( ("(", 
                                                  (op^ ( ((op^ ( ((op^ ( ("(", 
                                                                          (if
                                                                           t22
                                                                           then
                                                                           "true"
                                                                           else
                                                                           "false")) )), 
                                                                  (op^ ( (", ", 
                                                                          (Int.toString ( t23 ))) ))) )), 
                                                          ")") ))) )), 
                                          (op^ ( (", ", 
                                                  (op^ ( ((op^ ( ("\"", 
                                                                  t24) )), 
                                                          "\"") ))) ))) )), 
                                  ")") ))) ( ((true, 150), 
                                              "stan") )), 
                   "((true, 150), \"stan\")"), 
                  (((fn {x = t26, y = t27} =>
                          (op^ ( ((op^ ( ((op^ ( ((op^ ( ((op^ ( ((op^ ( ((op^ ( ((op^ ( ("{", 
                                                                                          "x") )), 
                                                                                  " = ") )), 
                                                                          (if
                                                                           t26
                                                                           then
                                                                           "true"
                                                                           else
                                                                           "false")) )), 
                                                                  ", ") )), 
                                                          "y") )), 
                                                  " = ") )), 
                                          (Int.toString ( t27 ))) )), 
                                  "}") ))) ( {x = true, y = 150} )), 
                   "{x = true, y = 150}"), 
                  (((fn {x = t29, w = {y = t31, z = t32}} =>
                          (op^ ( ((op^ ( ((op^ ( ((op^ ( ((op^ ( ((op^ ( ((op^ ( ((op^ ( ("{", 
                                                                                          "x") )), 
                                                                                  " = ") )), 
                                                                          (if
                                                                           t29
                                                                           then
                                                                           "true"
                                                                           else
                                                                           "false")) )), 
                                                                  ", ") )), 
                                                          "w") )), 
                                                  " = ") )), 
                                          (op^ ( ((op^ ( ((op^ ( ((op^ ( ((op^ ( ((op^ ( ((op^ ( ((op^ ( ("{", 
                                                                                                          "y") )), 
                                                                                                  " = ") )), 
                                                                                          (Int.toString ( t31 ))) )), 
                                                                                  ", ") )), 
                                                                          "z") )), 
                                                                  " = ") )), 
                                                          (op^ ( ((op^ ( ("\"", 
                                                                          t32) )), 
                                                                  "\"") ))) )), 
                                                  "}") ))) )), 
                                  "}") ))) ( {x = true, 
                                              w = {y = 150, 
                                                   z = "stan"}} )), 
                   "{x = true, w = {y = 150, z = \"stan\"}}"), 
                  (((fn {w = {x = t35, y = t36}, z = t37} =>
                          (op^ ( ((op^ ( ((op^ ( ((op^ ( ((op^ ( ((op^ ( ((op^ ( ((op^ ( ("{", 
                                                                                          "w") )), 
                                                                                  " = ") )), 
                                                                          (op^ ( ((op^ ( ((op^ ( ((op^ ( ((op^ ( ((op^ ( ((op^ ( ((op^ ( ("{", 
                                                                                                                                          "x") )), 
                                                                                                                                  " = ") )), 
                                                                                                                          (if
                                                                                                                           t35
                                                                                                                           then
                                                                                                                           "true"
                                                                                                                           else
                                                                                                                           "false")) )), 
                                                                                                                  ", ") )), 
                                                                                                          "y") )), 
                                                                                                  " = ") )), 
                                                                                          (Int.toString ( t36 ))) )), 
                                                                                  "}") ))) )), 
                                                                  ", ") )), 
                                                          "z") )), 
                                                  " = ") )), 
                                          (op^ ( ((op^ ( ("\"", 
                                                          t37) )), 
                                                  "\"") ))) )), 
                                  "}") ))) ( {w = {x = true, 
                                                   y = 150}, 
                                              z = "stan"} )), 
                   "{w = {x = true, y = 150}, z = \"stan\"}"), 
                  (((fn t38 =>
                          (case t38 of
                             NONE => "NONE"
                           | (SOME x) =>
                               (op^ ( ((op^ ( ("SOME (", 
                                               ((fn (t40, t41) =>
                                                      (op^ ( ((op^ ( ((op^ ( ("(", 
                                                                              (Int.toString ( t40 ))) )), 
                                                                      (op^ ( (", ", 
                                                                              (Int.toString ( t41 ))) ))) )), 
                                                              ")") ))) ( x ))) )), 
                                       ")") )))) ( (SOME ( (1, 
                                                            50) )) )), 
                   "SOME ((1, 50))"), 
                  (((fn _ => "<fn>") ( (fn x => x) )), "<fn>"), 
                  (((fn t43 =>
                          (case t43 of
                             NONE => "NONE"
                           | (SOME x) =>
                               (op^ ( ((op^ ( ("SOME (", 
                                               ((fn _ => "<fn>") ( x ))) )), 
                                       ")") )))) ( (SOME ( (fn x =>
                                                                 x) )) )), 
                   "SOME (<fn>)")], 
                 check_eq) ))
  end