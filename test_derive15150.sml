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
    fun show_simple1 t48 = (case t48 of A => "A" | B => "B")
    and simple1_show x = (show_simple1 ( x ))
    datatype const =
      Int of int
    | Real of real
    | Char of char
    | String of string
    | Bool of bool
    (* [.deriving show] *)
    fun show_const t54 =
        (case t54 of
           (Int t49) =>
             (op^ ( ((op^ ( ("Int (", (Int.toString ( t49 ))) )), 
                     ")") ))
         | (Real t50) =>
             (op^ ( ((op^ ( ("Real (", (Real.toString ( t50 ))) )), 
                     ")") ))
         | (Char t51) =>
             (op^ ( ((op^ ( ("Char (", 
                             (op^ ( ((op^ ( ("#\"", 
                                             (Char.toString ( t51 ))) )), 
                                     "\"") ))) )), 
                     ")") ))
         | (String t52) =>
             (op^ ( ((op^ ( ("String (", 
                             (op^ ( ((op^ ( ("\"", t52) )), "\"") ))) )), 
                     ")") ))
         | (Bool t53) =>
             (op^ ( ((op^ ( ("Bool (", 
                             (if t53 then "true" else "false")) )), 
                     ")") )))
    and const_show x = (show_const ( x ))
    datatype 'a poly = Poly of 'a  (* [.deriving show] *)
    fun show_poly fn0 t56 =
        (case t56 of
           (Poly t55) =>
             (op^ ( ((op^ ( ("Poly (", (fn0 ( t55 ))) )), ")") ))
         )
    and poly_show x = (show_poly ( x ))
    datatype mono =
      Mono1 of int list
    | Mono2 of int option
    | Mono3 of int poly
    (* [.deriving show] *)
    fun show_mono t63 =
        (case t63 of
           (Mono1 t57) =>
             (op^ ( ((op^ ( ("Mono1 (", 
                             (op^ ( ((op^ ( ("[", 
                                             ((String.concatWith ( ", " )) ( ((List.map ( (fn t58 =>
                                                                                                (Int.toString ( t58 ))) )) ( t57 )) ))) )), 
                                     "]") ))) )), 
                     ")") ))
         | (Mono2 t59) =>
             (op^ ( ((op^ ( ("Mono2 (", 
                             (case t59 of
                                NONE => "NONE"
                              | (SOME x) =>
                                  (op^ ( ((op^ ( ("SOME (", 
                                                  ((fn t60 =>
                                                         (Int.toString ( t60 ))) ( x ))) )), 
                                          ")") )))) )), 
                     ")") ))
         | (Mono3 t61) =>
             (op^ ( ((op^ ( ("Mono3 (", 
                             ((poly_show ( (fn t62 =>
                                                 (Int.toString ( t62 ))) )) ( t61 ))) )), 
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
                          (case t10 of
                             LESS => "LESS"
                           | EQUAL => "EQUAL"
                           | GREATER => "GREATER")) ( LESS )), 
                   "LESS"), 
                  (((fn t11 =>
                          (case t11 of
                             LESS => "LESS"
                           | EQUAL => "EQUAL"
                           | GREATER => "GREATER")) ( EQUAL )), 
                   "EQUAL"), 
                  (((fn t12 =>
                          (case t12 of
                             LESS => "LESS"
                           | EQUAL => "EQUAL"
                           | GREATER => "GREATER")) ( GREATER )), 
                   "GREATER"), 
                  (((fn t13 =>
                          ((poly_show ( (fn t14 =>
                                              (Int.toString ( t14 ))) )) ( t13 ))) ( (Poly ( 150 )) )), 
                   "Poly (150)"), 
                  (((fn (t16, t17) =>
                          (op^ ( ((op^ ( ("(", 
                                          (op^ ( ((if
                                                   t16
                                                   then
                                                   "true"
                                                   else "false"), 
                                                  (op^ ( (", ", 
                                                          (Int.toString ( t17 ))) ))) ))) )), 
                                  ")") ))) ( (true, 150) )), 
                   "(true, 150)"), 
                  (((fn (t19, (t21, t22)) =>
                          (op^ ( ((op^ ( ("(", 
                                          (op^ ( ((if
                                                   t19
                                                   then
                                                   "true"
                                                   else "false"), 
                                                  (op^ ( (", ", 
                                                          (op^ ( ((op^ ( ("(", 
                                                                          (op^ ( ((Int.toString ( t21 )), 
                                                                                  (op^ ( (", ", 
                                                                                          (op^ ( ((op^ ( ("\"", 
                                                                                                          t22) )), 
                                                                                                  "\"") ))) ))) ))) )), 
                                                                  ")") ))) ))) ))) )), 
                                  ")") ))) ( (true, 
                                              (150, "stan")) )), 
                   "(true, (150, \"stan\"))"), 
                  (((fn ((t25, t26), t27) =>
                          (op^ ( ((op^ ( ("(", 
                                          (op^ ( ((op^ ( ((op^ ( ("(", 
                                                                  (op^ ( ((if
                                                                           t25
                                                                           then
                                                                           "true"
                                                                           else
                                                                           "false"), 
                                                                          (op^ ( (", ", 
                                                                                  (Int.toString ( t26 ))) ))) ))) )), 
                                                          ")") )), 
                                                  (op^ ( (", ", 
                                                          (op^ ( ((op^ ( ("\"", 
                                                                          t27) )), 
                                                                  "\"") ))) ))) ))) )), 
                                  ")") ))) ( ((true, 150), 
                                              "stan") )), 
                   "((true, 150), \"stan\")"), 
                  (((fn {x = t29, y = t30} =>
                          (op^ ( ((op^ ( ("{", 
                                          (op^ ( ((op^ ( ((op^ ( ((op^ ( ("x", 
                                                                          " = ") )), 
                                                                  (if
                                                                   t29
                                                                   then
                                                                   "true"
                                                                   else
                                                                   "false")) )), 
                                                          ", ") )), 
                                                  (op^ ( ((op^ ( ("y", 
                                                                  " = ") )), 
                                                          (Int.toString ( t30 ))) ))) ))) )), 
                                  "}") ))) ( {x = true, y = 150} )), 
                   "{x = true, y = 150}"), 
                  (((fn {x = t32, w = {y = t34, z = t35}} =>
                          (op^ ( ((op^ ( ("{", 
                                          (op^ ( ((op^ ( ((op^ ( ((op^ ( ("x", 
                                                                          " = ") )), 
                                                                  (if
                                                                   t32
                                                                   then
                                                                   "true"
                                                                   else
                                                                   "false")) )), 
                                                          ", ") )), 
                                                  (op^ ( ((op^ ( ("w", 
                                                                  " = ") )), 
                                                          (op^ ( ((op^ ( ("{", 
                                                                          (op^ ( ((op^ ( ((op^ ( ((op^ ( ("y", 
                                                                                                          " = ") )), 
                                                                                                  (Int.toString ( t34 ))) )), 
                                                                                          ", ") )), 
                                                                                  (op^ ( ((op^ ( ("z", 
                                                                                                  " = ") )), 
                                                                                          (op^ ( ((op^ ( ("\"", 
                                                                                                          t35) )), 
                                                                                                  "\"") ))) ))) ))) )), 
                                                                  "}") ))) ))) ))) )), 
                                  "}") ))) ( {x = true, 
                                              w = {y = 150, 
                                                   z = "stan"}} )), 
                   "{x = true, w = {y = 150, z = \"stan\"}}"), 
                  (((fn {w = {x = t38, y = t39}, z = t40} =>
                          (op^ ( ((op^ ( ("{", 
                                          (op^ ( ((op^ ( ((op^ ( ((op^ ( ("w", 
                                                                          " = ") )), 
                                                                  (op^ ( ((op^ ( ("{", 
                                                                                  (op^ ( ((op^ ( ((op^ ( ((op^ ( ("x", 
                                                                                                                  " = ") )), 
                                                                                                          (if
                                                                                                           t38
                                                                                                           then
                                                                                                           "true"
                                                                                                           else
                                                                                                           "false")) )), 
                                                                                                  ", ") )), 
                                                                                          (op^ ( ((op^ ( ("y", 
                                                                                                          " = ") )), 
                                                                                                  (Int.toString ( t39 ))) ))) ))) )), 
                                                                          "}") ))) )), 
                                                          ", ") )), 
                                                  (op^ ( ((op^ ( ("z", 
                                                                  " = ") )), 
                                                          (op^ ( ((op^ ( ("\"", 
                                                                          t40) )), 
                                                                  "\"") ))) ))) ))) )), 
                                  "}") ))) ( {w = {x = true, 
                                                   y = 150}, 
                                              z = "stan"} )), 
                   "{w = {x = true, y = 150}, z = \"stan\"}"), 
                  (((fn t41 =>
                          (case t41 of
                             NONE => "NONE"
                           | (SOME x) =>
                               (op^ ( ((op^ ( ("SOME (", 
                                               ((fn (t43, t44) =>
                                                      (op^ ( ((op^ ( ("(", 
                                                                      (op^ ( ((Int.toString ( t43 )), 
                                                                              (op^ ( (", ", 
                                                                                      (Int.toString ( t44 ))) ))) ))) )), 
                                                              ")") ))) ( x ))) )), 
                                       ")") )))) ( (SOME ( (1, 
                                                            50) )) )), 
                   "SOME ((1, 50))"), 
                  (((fn _ => "<fn>") ( (fn x => x) )), "<fn>"), 
                  (((fn t46 =>
                          (case t46 of
                             NONE => "NONE"
                           | (SOME x) =>
                               (op^ ( ((op^ ( ("SOME (", 
                                               ((fn _ => "<fn>") ( x ))) )), 
                                       ")") )))) ( (SOME ( (fn x =>
                                                                 x) )) )), 
                   "SOME (<fn>)")], 
                 check_eq) ))
  end