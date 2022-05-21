
datatype test =
    Con1
  | Con2 [@@deriving eq]


structure Test =
  struct
    type t = int [@@deriving eq]
  end
