
structure CM_Token =
  struct
    type symbol = Symbol.symbol
    type span = Span.span

    datatype elem =
      PATH of symbol Node.t
    | STRING of string Node.t

    datatype token =
      ELEM of elem

    | LIBRARY of span
    | GROUP of span
    | IS of span

    | EOF of span

    val node_eq = Node.location_insensitive_eq
    fun insensitive_compare token1 token2 =
      case (token1, token2) of
        (ELEM (PATH node1), ELEM (PATH node2)) => node_eq Symbol.eq (node1, node2)

      | (ELEM (STRING node1), ELEM (STRING node2)) => node_eq (op=) (node1, node2)

      | (EOF _, EOF _) => true
      | _ => false

    fun elem_to_string elem =
      case elem of
        PATH node => Symbol.toValue (Node.getVal node)
      | STRING node => Node.getVal node

    fun token_to_string tok =
      case tok of
        ELEM elem => elem_to_string elem

      | LIBRARY _ => "Library"
      | GROUP _ => "Group"
      | IS _ => "Is"

      | EOF _ => "eof"

  end
