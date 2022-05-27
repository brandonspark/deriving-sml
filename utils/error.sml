
datatype warning =
    ParseWarning of (string * string list)
  | InvalidExt of string
  | InvalidFile of string
  | LexWarning of
       { filename : string
       , reason : string
       , pos : int
       , rest : char list
       }
  | TransformWarning of
       { filename : string
       , reason : string
       , pos : int
       }
  | GeneralWarning of
       { filename : string
       , reason : string
       , span : (int * int)
       }

datatype error =
    ParseError of (string * string list)
  | TransformError of
      { reason : string
      , pos : int
      }
  | LexError of
       { reason : string
       , pos : int
       , rest : char list
       }
  | ExpectedIdent of
       { expected : string
       , got : string
       , span : (int * int)
       }
  | FixityError of
       { reason : string
       , span : (int * int)
       }

datatype signal =
    SigError of error
  | SigWarn of warning

signature ERROR =
  sig

    datatype warning = datatype warning
    datatype error = datatype error
    datatype signal = datatype signal

    exception Signal of signal

    val warn : ('a -> (warning -> 'a))

    val err : (error -> 'a)
  end
structure Error :> ERROR =
  struct

    datatype warning = datatype warning
    datatype error = datatype error
    datatype signal = datatype signal

    exception Signal of signal

    fun warn x warning =
      ( print
        ( case warning of
            ParseWarning (filename, rest) =>
               "failed to parse file " ^ filename ^ ", skipping\n" ^ "remaining: "
               ^ String.substring ( String.concatWith " " rest, 0, 25) ^ "\n"
          | (InvalidExt filename) =>
              "Invalid extension: " ^ filename ^ "\n"

          | (InvalidFile filename) =>
              "Invalid file: " ^ filename ^ "\n"

          | (LexWarning {filename, reason, pos, rest}) =>
              "temp\n"
        )
      ; x
      )

    fun err error = raise Signal (SigError error)
  end
