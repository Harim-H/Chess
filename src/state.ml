open Piece
open Square
open Board
open Helper

exception UndoFailure

let rec gen_int start finish =
  if start > finish then [] else start :: gen_int (start + 1) finish

let pawn_locations (color : color) : point list =
  let y = if color = White then 2 else 7 in
  List.map (fun x -> (x, y)) (gen_int 1 8)

let pawn_pieces (color : color) : piece list =
  List.map
    (fun loc ->
      { piece_type = Pawn pawn; location = loc; color; first_move = true })
    (pawn_locations color)

let other_piece_locations (color : color) : point list =
  let y = if color = White then 1 else 8 in
  List.map (fun x -> (x, y)) (gen_int 1 8)

let other_piece_type =
  [
    Rook rook;
    Knight knight;
    Bishop bishop;
    Queen queen;
    King king;
    Bishop bishop;
    Knight knight;
    Rook rook;
  ]

let other_pieces (color : color) : piece list =
  let t_and_loc = List.combine other_piece_type (other_piece_locations color) in
  List.map
    (fun (t, loc) ->
      { piece_type = t; location = loc; color; first_move = true })
    t_and_loc

let generate_pieces (color : color) : piece list =
  List.append (pawn_pieces color) (other_pieces color)

let gen_white_start_pos = rowGen 1 @ rowGen 2
let gen_black_start_pos = rowGen 7 @ rowGen 8

let random_piece =
  Random.self_init ();
  fun () ->
    let r = Random.int 15 in
    if r <= 7 then Pawn pawn
    else if 7 < r && r <= 9 then Rook rook
    else if 9 < r && r <= 11 then Knight knight
    else if 11 < r && r <= 13 then Bishop bishop
    else Queen queen

let anarchy_generate_pieces color position : piece list =
  List.map
    (fun loc ->
      if loc = (5, 1) || loc = (5, 8) then
        { piece_type = King king; location = loc; color; first_move = true }
      else
        {
          piece_type = random_piece ();
          location = loc;
          color;
          first_move = true;
        })
    position

let pieces = List.append (generate_pieces White) (generate_pieces Black)

let anarchy_pieces =
  List.append
    (anarchy_generate_pieces White gen_white_start_pos)
    (anarchy_generate_pieces Black gen_black_start_pos)

let get_init_board = init_board pieces
let get_init_anar_board = init_board anarchy_pieces

let get_first_state (state_lst : Square.t Board.t list) : Square.t Board.t =
  match state_lst with
  | [] -> failwith "Invalid state. State should never be empty"
  | h :: _ -> h

let move_piece ((start, dest) : point * point)
    (state_lst : Square.t Board.t list) : bool * Square.t Board.t list =
  let h = get_first_state state_lst in
  let _, new_board = update_piece_loc start dest h in
  (true, new_board :: state_lst)

let revert_state (state_lst : Square.t Board.t list) =
  match state_lst with
  | [] | [ _ ] | [ _; _ ] -> raise UndoFailure
  | _ :: _ :: t -> t
