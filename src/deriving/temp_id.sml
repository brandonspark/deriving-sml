
structure TempId :
  sig
    val new : unit -> SMLSyntax.identifier
  end =
  struct
    val counter = ref 0

    fun new () =
      let
        val cur = !counter
      in
        counter := !counter + 1;
        Node.create (Symbol.fromValue ("t" ^ Int.toString cur), Span.absurd)
      end


  end
