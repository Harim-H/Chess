open Piece
open Square

module Point = struct
  type t = int * int

  let compare (x1, y1) (x2, y2) =
    match Stdlib.compare x1 x2 with
    | 0 -> Stdlib.compare y1 y2
    | c -> c
end

module Board = Map.Make (Point)

let rec init_board pieces =
  match pieces with
  | [] -> Board.empty
  | h :: t ->
      init_board t
      |> Board.add h.location
           Square.{ coordinate = h.location; has_piece = Some h }

let has_piece = Board.mem

let get_piece point (board : Square.t Board.t) =
  try
    match Board.find point board with
    | { has_piece; _ } -> has_piece
  with Not_found -> None

let update_piece_loc point loc (board : Square.t Board.t) =
  match get_piece point board with
  | None -> (None, board)
  | Some p ->
      let cap_piece = get_piece loc board in
      ( cap_piece,
        Board.remove point board
        |> Board.add loc
             Square.
               {
                 coordinate = loc;
                 has_piece = Some { p with location = loc; first_move = false };
               } )

let get_piece_list (color : color) (board : Square.t Board.t) =
  List.map
    (fun (_, s) ->
      match s with
      | Square.{ has_piece; _ } -> has_piece)
    (Board.bindings board)
  |> List.filter (fun p ->
         match p with
         | None -> false
         | Some piece -> piece.color = color)

let empty = Board.empty

let add_piece p board =
  let loc = p.location in
  Board.remove loc board
  |> Board.add loc Square.{ coordinate = loc; has_piece = Some p }

let add_pieces p_lst board =
  List.fold_left (fun acc p -> add_piece p acc) board p_lst

let remove_piece = Board.remove

let enp_capturing_fal (start : point) (endpoint : point)
    (board : Square.t Board.t) : bool =
  let p = get_piece start board in
  match p with
  | None -> true
  | Some p -> (
      (p.location |> snd <> if p.color = Black then 4 else 5)
      ||
      match p.piece_type with
      | Pawn _ ->
          false
          ||
          let offset = if p.color = White then -1 else 1 in
          (start |> fst) - (endpoint |> fst) |> abs <> 1
          || (start |> snd) - (endpoint |> snd) <> offset
      | _ -> true)

let pawn_at_point (loc : point) (board : Square.t Board.t) =
  if has_piece loc board then
    match get_piece loc board with
    | None -> false
    | Some p -> (
        match p.piece_type with
        | Pawn _ -> true
        | _ -> false)
  else false

let enp_capturing (captured : piece) (prev : Square.t Board.t)
    (curr : Square.t Board.t) : bool =
  match captured.piece_type with
  | Pawn _ ->
      let dir = if captured.color = Black then 2 else -2 in
      let loc = (captured.location |> fst, (captured.location |> snd) + dir) in
      pawn_at_point loc prev && pawn_at_point captured.location curr
  | _ -> false

let check_enpassant (start : point) (endpoint : point)
    (board_lst : Square.t Board.t list) : bool =
  match board_lst with
  | [] | [ _ ] | [ _; _ ] -> false
  | curr :: _ :: prev :: _ ->
      if not (has_piece start curr) then false
      else if enp_capturing_fal start endpoint curr then false
      else
        let check =
          match
            ( get_piece start curr,
              get_piece (endpoint |> fst, start |> snd) curr )
          with
          | Some s, Some e ->
              if s.color = e.color then false else enp_capturing e prev curr
          | _ -> false
        in
        check

let remove_enp_state (board_lst : Square.t Board.t list) : Square.t Board.t list
    =
  match board_lst with
  | [] | [ _ ] -> board_lst
  | f :: _ :: t -> f :: t

let check_end_piece_ownership (piece : piece) (end_point : point)
    (board : Square.t Board.t) =
  let end_piece = get_piece end_point board in
  match end_piece with
  | None -> false
  | Some p -> p.color = piece.color
