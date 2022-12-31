open Piece
open Square
open Board
open Helper

let piece_type_color_to_string p =
  match p with
  | Some { piece_type = Pawn _; color = Black; _ } -> "♟"
  | Some { piece_type = Rook _; color = Black; _ } -> "♜"
  | Some { piece_type = Knight _; color = Black; _ } -> "♞"
  | Some { piece_type = Bishop _; color = Black; _ } -> "♝"
  | Some { piece_type = Queen _; color = Black; _ } -> "♛"
  | Some { piece_type = King _; color = Black; _ } -> "♚"
  | Some { piece_type = Pawn _; color = White; _ } -> "♙"
  | Some { piece_type = Rook _; color = White; _ } -> "♖"
  | Some { piece_type = Knight _; color = White; _ } -> "♘"
  | Some { piece_type = Bishop _; color = White; _ } -> "♗"
  | Some { piece_type = Queen _; color = White; _ } -> "♕"
  | Some { piece_type = King _; color = White; _ } -> "♔"
  | None -> ""

let rec print_rows pts board =
  match pts with
  | [] -> ""
  | h :: t ->
      if has_piece h board then
        " "
        ^ piece_type_color_to_string (get_piece h board)
        ^ "  ║" ^ print_rows t board
      else "    ║" ^ print_rows t board

let row_split_string = "\n    ╠════╬════╬════╬════╬════╬════╬════╬════╣"

let letter = "      a    b    c    d    e    f    g    h"

let top_board_string = "\n    ╔════╦════╦════╦════╦════╦════╦════╦════╗"

let bottom_board_string = "\n    ╚════╩════╩════╩════╩════╩════╩════╩════╝ \n"

let print_row_splt_line pts board =
  match List.hd pts with
  | _, 1 ->
      "\n 1  ║" ^ print_rows pts board ^ "  1" ^ bottom_board_string ^ letter
  | _, 2 -> "\n 2  ║" ^ print_rows pts board ^ "  2" ^ row_split_string
  | _, 3 -> "\n 3  ║" ^ print_rows pts board ^ "  3" ^ row_split_string
  | _, 4 -> "\n 4  ║" ^ print_rows pts board ^ "  4" ^ row_split_string
  | _, 5 -> "\n 5  ║" ^ print_rows pts board ^ "  5" ^ row_split_string
  | _, 6 -> "\n 6  ║" ^ print_rows pts board ^ "  6" ^ row_split_string
  | _, 7 -> "\n 7  ║" ^ print_rows pts board ^ "  7" ^ row_split_string
  | _, 8 ->
      "\n" ^ letter ^ top_board_string ^ "\n 8  ║" ^ print_rows pts board
      ^ "  8" ^ row_split_string
  | _, _ -> ""

let print_board board =
  let acc = ref "" in
  for i = 1 to 8 do
    acc := print_row_splt_line (rowGen i) board ^ !acc
  done;
  !acc