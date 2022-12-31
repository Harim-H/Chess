open Piece

type opt_piece = Some of piece | None

module Square = struct
  type t = { coordinate : int * int; has_piece : opt_piece }

  let compare s1 s2 =
    match (s1, s2) with
    | { coordinate = x1, y1; _ }, { coordinate = x2, y2; _ } -> (
        match Stdlib.compare x1 x2 with
        | 0 -> Stdlib.compare y1 y2
        | c -> c)
end