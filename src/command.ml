open Piece
open Square
open State
open Board
open Helper

exception Empty
exception Malformed

type go_location = piece * string list
type command = Go of go_location | Quit

let capture = "Capturing"

let rec has_piece_in_path (start_point : point) (end_point : point)
    (board : Square.t Board.t) (direction : point) : bool =
  if start_point = end_point then false
  else if
    has_piece
      (fst start_point + fst direction, snd start_point + snd direction)
      board
  then true
  else
    has_piece_in_path
      (fst start_point + fst direction, snd start_point + snd direction)
      end_point board direction

let direction x y =
  match Stdlib.compare x y with
  | v when v > 0 -> -1
  | v when v < 0 -> 1
  | _ -> 0

let illegal_jump (capturing : bool) (piece : piece) (end_point : point)
    (board : Square.t Board.t) =
  match piece.piece_type with
  | Pawn _ ->
      let sign = if piece.color = White then 1 else -1 in
      let x, y = (fst piece.location, snd piece.location) in
      if capturing then not (has_piece end_point board)
      else
        has_piece (x, y + sign) board
        ||
        if snd end_point - y = 2 then has_piece (x, y + (2 * sign)) board
        else false
  | Knight _ | King _ ->
      (capturing && not (has_piece end_point board))
      || ((not capturing) && has_piece end_point board)
  | Rook _ | Bishop _ | Queen _ ->
      let x_dir = direction (fst piece.location) (fst end_point) in
      let y_dir = direction (snd piece.location) (snd end_point) in
      let check_endpoint =
        if capturing && has_piece end_point board then
          (fst end_point + (-1 * x_dir), snd end_point + (-1 * y_dir))
        else end_point
      in
      has_piece_in_path piece.location check_endpoint board (x_dir, y_dir)

let is_move_available (capturing : bool) (piece : piece) (end_point : point) =
  match piece.piece_type with
  | Pawn _ ->
      let sign = if piece.color = White then 1 else -1 in
      let x_dif, y_dif =
        (fst end_point - fst piece.location, snd end_point - snd piece.location)
      in
      if capturing && List.mem (x_dif, y_dif) [ (-1, sign); (1, sign) ] then
        true
      else
        (x_dif, y_dif) = (0, 2 * sign)
        && piece.first_move = true && not capturing
        || ((not capturing) && List.mem (x_dif, y_dif) [ (0, sign) ])
  | _ ->
      List.mem
        (fst end_point - fst piece.location, snd end_point - snd piece.location)
        (get_moves piece.piece_type)

let is_move_beyond_boundary (end_point : point) =
  fst end_point > 8
  || fst end_point < 0
  || snd end_point > 8
  || snd end_point < 0

let is_legal_move (capturing : bool) (piece : piece) (end_point : point)
    (board : Square.t Board.t) : bool =
  if not (is_move_available capturing piece end_point) then false
  else if check_end_piece_ownership piece end_point board then false
  else if is_move_beyond_boundary end_point then false
  else if illegal_jump capturing piece end_point board then false
  else true

let split_str (str : string) =
  if String.empty = str then raise Empty
  else if String.contains str 'x' then
    capture :: String.split_on_char 'x' (String.trim str)
  else if String.contains str '-' then [ str ]
  else
    [
      String.sub str 0 (String.length str - 2);
      String.sub str (String.length str - 2) 2;
    ]

let end_coord (str : string) =
  if String.length str != 2 then
    failwith "coordinate need to be be exactly 2 chars"
  else if int_of_char str.[1] - 48 > 9 || int_of_char str.[1] - 48 < 1 then
    failwith "y val is out of range"
  else (letter_to_number str.[0], int_of_char str.[1] - 48)

let piece_code_to_general (piece_code : string) =
  if piece_code = "" then Pawn pawn
  else
    match piece_code.[0] with
    | 'R' -> Rook rook
    | 'N' -> Knight knight
    | 'B' -> Bishop bishop
    | 'Q' -> Queen queen
    | 'K' -> King king
    | _ -> raise Malformed

let rec list_general_piece (p_type : general_piece) (pieces : piece list)
    (acc : piece list) =
  match pieces with
  | [] -> acc
  | h :: t ->
      if p_type = h.piece_type then list_general_piece p_type t (h :: acc)
      else list_general_piece p_type t acc

let rec list_potential_pieces (capturing : bool) (endpoint : int * int)
    (pieces : piece list) (acc : piece list) (board : Square.t Board.t) =
  match pieces with
  | [] -> acc
  | h :: t ->
      if is_legal_move capturing h endpoint board then
        list_potential_pieces capturing endpoint t (h :: acc) board
      else list_potential_pieces capturing endpoint t acc board

let spec_coord_noncap (input_lst : string list) =
  let input = List.fold_left (fun acc x -> acc ^ x) "" input_lst in
  let s = List.hd (split_str input) in
  if String.length s >= 2 then
    if String.length s == 2 then
      let n = letter_to_number s.[1] in
      (n, n)
    else (letter_to_number s.[1], int_of_char s.[2])
  else failwith "Input string doesn't spec file as needed"

let spec_coord_cap (input_lst : string list) : int * int =
  let input = List.fold_left (fun acc x -> acc ^ x) "" (List.tl input_lst) in
  let s = List.hd (split_str input) in
  if
    List.filter (fun x -> x = s.[0]) [ 'Q'; 'K'; 'N'; 'B'; 'R' ]
    |> List.length <> 0
  then
    match String.length s with
    | n when n < 2 -> (-1, -1)
    | n when n == 2 ->
        let n = letter_to_number s.[1] in
        (n, n)
    | _ -> (letter_to_number s.[1], int_of_char s.[2])
  else (letter_to_number (String.get s 0), -1)

let rec matching_coord_piece (lst : piece list) (f : int * int -> int)
    (coord : int) (acc : piece list) =
  match lst with
  | [] -> acc
  | h :: t ->
      if f h.location = coord then matching_coord_piece t f coord (h :: acc)
      else matching_coord_piece t f coord acc

let rec specific_piece (input : string list) (pieces_list : piece list)
    (board : Square.t Board.t) =
  let capturing = List.hd input = capture in
  ();
  let input_lst =
    if capturing then
      match input with
      | _ :: t -> (
          match List.nth input 1 with
          | s when check_not_pawn s -> t
          | _ -> "" :: t)
      | [] -> raise Empty
    else input
  in
  let lst =
    list_potential_pieces capturing
      (end_coord (List.hd (List.rev input)))
      (list_general_piece
         (piece_code_to_general (List.hd input_lst))
         pieces_list [])
      [] board
  in
  if List.length lst = 0 then failwith "no pieces can move here"
  else if List.length lst = 1 then List.hd lst
  else filter_piece capturing input lst

and check_not_pawn s =
  String.contains s 'R' || String.contains s 'Q' || String.contains s 'B'
  || String.contains s 'N' || String.contains s 'K'

and filter_piece capturing input lst =
  (if capturing then spec_coord_cap input else spec_coord_noncap input)
  |> fun (x, y) ->
  let p = if x = -1 then lst else matching_coord_piece lst fst x [] in
  if List.length p == 1 then List.hd p
  else
    let p' = matching_coord_piece p fst y [] in
    if List.length p' == 1 then List.hd p'
    else failwith "piece not specified enough"

let start_end_coord (input : string) (pieces_list : piece list)
    (board : Square.t Board.t) =
  let lst = split_str input in
  [
    (specific_piece lst pieces_list board).location;
    end_coord (List.hd (List.rev lst));
  ]

let enpassant_points (input : string) (color : color) =
  let lst = split_str input in
  match lst with
  | [] | [ _ ] -> raise Malformed
  | _ :: c :: t ->
      let row = if color = Black then 4 else 5 in
      let start_coord = (letter_to_number (String.get c 0), row) in
      let end_coord = end_coord (List.hd (List.rev t)) in
      (start_coord, end_coord)

let input_enpassant (input : string) (board : Square.t Board.t) (turn : color) :
    bool =
  let lst = split_str input in
  match lst with
  | [] -> false
  | h :: t ->
      h = capture
      && (match turn with
         | White -> String.contains input '6'
         | Black -> String.contains input '3')
      &&
      let endpoint = end_coord (List.hd (List.rev t)) in
      not (has_piece endpoint board)

(*Returns the king of the chosen color*)
let get_king st turn =
  List.hd
    (list_general_piece (King king) (unopt_pieces (get_piece_list turn st)) [])

(* Returns the list of pieces attacking the king of the chosen color*)
let list_of_attackers st turn =
  list_potential_pieces true (get_king st turn).location
    (unopt_pieces (get_piece_list (not_color turn) st))
    [] st

(*Checks whether the chosen color's side is in check or not*)
let is_in_check st turn = List.length (list_of_attackers st turn) > 0

(* Returns whether moving a piece will result in check in the next move*)
let rec move_is_check (st : Square.t Board.t) (turn : color)
    (pieces : piece list) (move : point) : bool =
  match pieces with
  | [] -> true
  | [ h ] ->
      is_in_check (move_piece (h.location, move) [ st ] |> snd |> List.hd) turn
  | h :: t ->
      if
        not
          (is_in_check
             (move_piece (h.location, move) [ st ] |> snd |> List.hd)
             turn)
      then false
      else move_is_check st turn t move

(* Returns whether a chosen side has no legal move*)
let has_no_legal_move st turn =
  let rec coordinates x y lst =
    if x > 8 then lst
    else if y > 8 then coordinates (x + 1) 1 lst
    else coordinates x (y + 1) ((x, y) :: lst)
  in
  let coords = coordinates 1 1 [] in
  let rec checking spaces =
    match spaces with
    | [] -> true
    | [ h ] ->
        if get_piece h st = None then
          move_is_check st turn
            (list_potential_pieces false h
               (unopt_pieces (get_piece_list turn st))
               [] st)
            h
        else
          move_is_check st turn
            (list_potential_pieces true h
               (unopt_pieces (get_piece_list turn st))
               [] st)
            h
    | h :: t ->
        if
          if get_piece h st = None then
            not
              (move_is_check st turn
                 (list_potential_pieces false h
                    (unopt_pieces (get_piece_list turn st))
                    [] st)
                 h)
          else
            not
              (move_is_check st turn
                 (list_potential_pieces true h
                    (unopt_pieces (get_piece_list turn st))
                    [] st)
                 h)
        then false
        else checking t
  in
  checking coords

(*Returns whether there is insufficient material for a checkmate*)
let insufficient_material st =
  let white_pieces =
    List.filter
      (fun x -> x.piece_type <> King king)
      (unopt_pieces (get_piece_list White st))
  in
  let black_pieces =
    List.filter
      (fun x -> x.piece_type <> King king)
      (unopt_pieces (get_piece_list Black st))
  in
  let checking lst =
    match lst with
    | [] -> true
    | h :: t ->
        if t <> [] then false
        else if h.piece_type = Bishop bishop || h.piece_type = Knight knight
        then true
        else false
  in
  if
    List.length white_pieces = 1
    && List.length black_pieces = 1
    && (List.hd white_pieces).piece_type = Bishop bishop
    && (List.hd black_pieces).piece_type = Bishop bishop
  then
    (fst (List.hd white_pieces).location + snd (List.hd white_pieces).location)
    mod 2
    = (fst (List.hd black_pieces).location + snd (List.hd black_pieces).location)
      mod 2
  else checking white_pieces && checking black_pieces

let board_edge_y_coord color =
  match color with
  | White -> 1
  | Black -> 8

let castle_helper board turn value =
  List.length
    (list_potential_pieces false
       (value, board_edge_y_coord turn)
       (unopt_pieces (get_piece_list (not_color turn) board))
       [] board)
  > 0

let rooksquare' turn board value =
  match get_piece (value, board_edge_y_coord turn) board with
  | None -> failwith "Rook must be on original square"
  | Some p -> p

let rooksquare_compare turn (rooksquare : piece) (value : int) =
  rooksquare.piece_type <> Rook rook
  || rooksquare.location <> (value, board_edge_y_coord turn)
  || rooksquare.color <> turn
  || rooksquare.first_move <> true

let shortcastle (board_lst : Square.t Board.t list) (turn : color) =
  let board = get_first_state board_lst in
  if castle_helper board turn 6 then failwith "Can't castle through check"
  else if castle_helper board turn 7 then failwith "Can't castle into check"
  else if
    has_piece_in_path
      (5, board_edge_y_coord turn)
      (7, board_edge_y_coord turn)
      board (1, 0)
  then failwith "Squares occupied"
  else
    let rooksquare = rooksquare' turn board 8 in
    if rooksquare_compare turn rooksquare 8 then
      failwith "Rook must be on original square"
    else
      snd
        (State.move_piece
           ((5, board_edge_y_coord turn), (7, board_edge_y_coord turn))
           board_lst)
      |> State.move_piece
           ((8, board_edge_y_coord turn), (6, board_edge_y_coord turn))

let longcastle (board_lst : Square.t Board.t list) (turn : color) =
  let board = get_first_state board_lst in
  if castle_helper board turn 4 then failwith "Can't castle through check"
  else if castle_helper board turn 3 then failwith "Can't castle into check"
  else if
    has_piece_in_path
      (5, board_edge_y_coord turn)
      (3, board_edge_y_coord turn)
      board (-1, 0)
  then failwith "Squares occupied"
  else
    let rooksquare = rooksquare' turn board 1 in
    if rooksquare_compare turn rooksquare 1 then
      failwith "Rook must be on original square"
    else
      snd
        (State.move_piece
           ((5, board_edge_y_coord turn), (3, board_edge_y_coord turn))
           board_lst)
      |> State.move_piece
           ((1, board_edge_y_coord turn), (4, board_edge_y_coord turn))

let castle (input : string) (board_lst : Square.t Board.t list) (turn : color) =
  let board = get_first_state board_lst in
  if is_in_check board turn then failwith "Can't castle in check"
  else if (get_king board turn).first_move = false then
    failwith "Can't castle after first move"
  else
    match input with
    | "O-O" | "0-0" -> shortcastle board_lst turn
    | "O-O-O" | "0-0-0" -> longcastle board_lst turn
    | _ -> failwith "Not a castling move"

let promote (st : Square.t Board.t list) (pt : point) (turn : color)
    (input : string) =
  let pro_piece = List.hd (List.rev (String.split_on_char '=' input)) in
  let code =
    if not (String.contains input '=') then Queen queen
    else
      match pro_piece with
      | "Q" -> Queen queen
      | "R" -> Rook rook
      | "N" -> Knight knight
      | "B" -> Bishop bishop
      | _ -> failwith "Invalid promotion"
  in
  match st with
  | [] -> []
  | _ :: t ->
      add_piece
        { piece_type = code; location = pt; color = turn; first_move = false }
        (get_first_state st)
      :: t
