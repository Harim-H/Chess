open Piece
open Square

(*This file stores multiple helper functions that are called in different
  files*)
let rowGen (col : int) =
  let acc = ref [] in
  for i = 8 downto 1 do
    acc := (i, col) :: !acc
  done;
  !acc

let letter_to_number letter =
  match letter with
  | 'a' -> 1
  | 'b' -> 2
  | 'c' -> 3
  | 'd' -> 4
  | 'e' -> 5
  | 'f' -> 6
  | 'g' -> 7
  | 'h' -> 8
  | _ -> -1

let not_color color =
  match color with
  | White -> Black
  | Black -> White

let opt_piece_to_piece (opt : opt_piece) =
  match opt with
  | Some p -> p
  | None -> failwith "Not a opt_piece"

let rec unopt_pieces (pieces : opt_piece list) : piece list =
  match pieces with
  | [] -> []
  | h :: t -> (
      match h with
      | None -> unopt_pieces t
      | Some p -> p :: unopt_pieces t)
