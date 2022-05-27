
signature PRECEDENCE =
   sig
      type t

      val parse : Context.t -> t SMLSyntax.juxta list -> Span.span -> t
   end


signature PRECEDENCE_ARG =
   sig
      type t_
      type t = t_ Node.t

      val apply : t -> t -> t
      (* val applyCurriedInfix : t -> t -> t -> t  (* infix operator first *)
                                                    *)
      val applyTupledInfix : SMLSyntax.identifier -> t -> t -> t   (* infix operator first *)
   end


functor PrecedenceFun (structure Arg : PRECEDENCE_ARG)
   :> PRECEDENCE
      where type t = Arg.t
   =
   struct
      open Context
      open Arg
      open Error

      structure Table = SymbolHashTable

      type symbol = Symbol.symbol
      type pos = Span.pos
      type span = Span.span

      datatype elem =
         Oper of SMLSyntax.identifier * int * assoc * span
       | Arg of t



      (* Is this juxtaposed element an identifier, or some other atomic element?
       *)
      fun resolve ctx jux =
         (case jux of
             SMLSyntax.Jident (id, exp) =>
                (case Context.lookup_infix (SMLSyntax.id_to_string id) ctx of
                    NONE => Arg exp

                  | SOME {assoc, precedence} =>
                      Oper (id, precedence, assoc, Node.getSpan id))

           | SMLSyntax.Jatom exp => Arg exp)


      fun applyInfix oper item1 item2 =
         (* (case mode of
             CURRIED => applyCurriedInfix oper item1 item2
           | TUPLED => *)
         applyTupledInfix oper item1 item2


      fun tighter prec prec' assoc assoc' pos =
         (case Int.compare (prec, prec') of
             GREATER => true

           | LESS => false

           | EQUAL =>
                (case (assoc, assoc') of
                    (Left, Left) => false
                  | (Right, Right) => true
                  | _ =>
                      err
                        ( TransformError
                            { reason =
                                "adjacent equal-precedence infix operators have opposite associativity"
                            , pos = pos
                            }
                        )
                )
          )


      (* A well-formed stack is a nonempty list, with alternating Oper and Arg elements,
         and with the last element an Arg. *)

      (* stack is well-formed and its first element is an Arg, returns same way *)
      fun flushStack prec assoc stack pos =
         (case stack of
             [Arg _] => stack

           | Arg item1 :: Oper (oper, prec', assoc', span) :: Arg item2 :: tail =>
                if
                   tighter prec prec' assoc assoc' pos
                then
                   stack
                else
                   let
                      val stack' =
                         Arg (applyInfix oper item2 item1) :: tail
                   in
                      flushStack prec assoc stack' pos
                   end

           | _ => raise (Fail "precondition"))


      (* stack is well-formed *)
      fun parseLoop table stack l =
         (case l of
             [] =>
                stack

           | jux :: rest =>
                let
                   val elem = resolve table jux
                in
                   (case elem of
                       Oper (_, prec, assoc, span) =>
                          (case stack of
                              [] => raise (Fail "ill-formed stack")

                            | Oper _ :: _ =>
                                err
                                  ( TransformError
                                      { reason = "misplaced infix operator"
                                      , pos = #1 span
                                      }
                                  )

                            | Arg _ :: _ =>
                                 let
                                    val stack' = flushStack prec assoc stack (#1 span)
                                 in
                                    parseLoop table (elem :: stack') rest
                                 end)

                     | Arg item =>
                          (case stack of
                              [] => raise (Fail "ill-formed stack")

                            | Arg item' :: tail =>
                                 parseLoop table (Arg (apply item' item) :: tail) rest

                            | Oper _ :: _ =>
                                 parseLoop table (Arg item :: stack) rest))
                end)


      (* parse takes a list of juxtaposed (pats/exps), some of which may be infix,
       * and then resolves which way they associate based on precedence
       *)
      fun parse table l (fullspan : Span.span) =
         (case l of
             [] => raise (Fail "excluded syntactically")

           (* solitary operator, don't treat as infix *)
           | [jux as SMLSyntax.Jident (_, e)] => e

           (* optimization *)
           | [SMLSyntax.Jatom item] => item

           | first :: rest =>
                (case resolve table first of
                    (* Can't start with an infix operator *)
                    Oper (_, _, _, span) =>
                      err
                        ( TransformError
                            { reason = "misplaced infix operator"
                            , pos = #1 span
                            }
                        )

                  | arg as (Arg _) =>
                       let
                          val stack = parseLoop table [arg] rest
                       in
                          (case stack of
                              Oper _ :: _ =>
                                err
                                  ( TransformError
                                      { reason = "missing infix argument"
                                      , pos = #2 fullspan
                                      }
                                  )

                            | [] =>
                                 raise (Fail "ill-formed stack")

                            | Arg _ :: _ =>
                                 let
                                    val stack' = flushStack ~1 Left stack (#2 fullspan)

                                    (* Since stack' is well-formed, begins with an Arg,
                                       and was just flushed with ~1 precedence, it must consist
                                       of exactly one Arg. *)
                                 in
                                    (case stack' of
                                        [Arg item] => item

                                      | _ => raise (Fail "impossible"))
                                 end)
                       end))

   end


structure ExpPrecedence =
   PrecedenceFun
   (structure Arg =
       struct
          open SMLSyntax

          type t_ = exp_
          type t = exp

          fun apply e1 e2 =
             Node.create ( Eapp {left=e1, right=e2}, Node.join_span e1 e2 )

          (*
          fun applyCurriedInfix oper e1 e2 =
             let
                val new_span = Node.join_span e1 e2
             in
               Node.create
                 ( Eapp
                     { left = Node.create (Eapp {left=oper, right=e1}, new_span)
                     , right = e2 }
                 , new_span )
             end
          *)

          fun applyTupledInfix oper e1 e2 =
             let
                val new_span = Node.join_span e1 e2
             in
               Node.create
                 ( Eapp
                     (* NOTE: Here, we change the name of the operator, so that
                      * it's not prefix-applied as its infix name.
                      *)
                     { left =
                         (* NOTE: I've fucked with the location data here.
                          *)
                         Node.create_absurd
                          (Eident { opp = false
                                , id = [Node.map (fn sym => map_sym sym (fn s => "op" ^ s)) oper]
                                }
                          )
                     , right = Node.create (Etuple [e1, e2], new_span) }
                 , new_span )
             end

       end)


structure PatPrecedence =
   PrecedenceFun
   (structure Arg =
       struct
          open SMLSyntax
          open Error

          type t_ = pat_
          type t = pat
          type pat = t

          fun apply (p1 : pat) p2 =
             (case Node.getVal p1 of
                 Pconstr {id, ...} =>
                    Node.create
                      ( Papp {id=id, atpat=p2}
                      , Node.join_span p1 p2)

               | _ =>
                   err
                     (TransformError
                       { reason = "pattern operator is not a constructor"
                       , pos = #1 (Node.getSpan p1)
                       }
                      )
              )

          (*
          fun applyCurriedInfix oper e1 e2 =
             raise (Error.SyntaxError
                     ("infix pattern operator is curried", #1 (Node.getSpan oper)))
          *)

          fun applyTupledInfix oper e1 e2 =
                    let
                      val new_span = Node.join_span e1 e2
                    in
                      Node.create
                        ( Papp { id = [Node.map (fn sym => map_sym sym (fn s => "op" ^ s)) oper]
                                   , atpat = Node.create (Ptuple [e1, e2], new_span) }
                        , new_span)
                    end
       end)
