type point = int * int

let pawn = [ (0, 1); (0, -1) ]

let rook =
  [
    (0, 1);
    (1, 0);
    (0, 2);
    (2, 0);
    (0, 3);
    (3, 0);
    (0, 4);
    (4, 0);
    (0, 5);
    (5, 0);
    (0, 6);
    (6, 0);
    (0, 7);
    (7, 0);
    (0, 8);
    (8, 0);
    (0, -1);
    (-1, 0);
    (0, -2);
    (-2, 0);
    (0, -3);
    (-3, 0);
    (0, -4);
    (-4, 0);
    (0, -5);
    (-5, 0);
    (0, -6);
    (-6, 0);
    (0, -7);
    (-7, 0);
    (0, -8);
    (-8, 0);
  ]

let knight =
  [ (1, 2); (2, 1); (2, -1); (1, -2); (-1, 2); (-2, -1); (-2, 1); (-1, -2) ]

let bishop =
  [
    (1, 1);
    (2, 2);
    (3, 3);
    (4, 4);
    (5, 5);
    (6, 6);
    (7, 7);
    (8, 8);
    (-1, 1);
    (-2, 2);
    (-3, 3);
    (-4, 4);
    (-5, 5);
    (-6, 6);
    (-7, 7);
    (-8, 8);
    (-1, -1);
    (-2, -2);
    (-3, -3);
    (-4, -4);
    (-5, -5);
    (-6, -6);
    (-7, -7);
    (-8, -8);
    (1, -1);
    (2, -2);
    (3, -3);
    (4, -4);
    (5, -5);
    (6, -6);
    (7, -7);
    (8, -8);
  ]

let queen = rook @ bishop

let king =
  [ (0, 1); (1, 0); (1, 1); (-1, 1); (0, -1); (-1, 0); (-1, -1); (1, -1) ]

type general_piece =
  | Pawn of point list
  | Rook of point list
  | Knight of point list
  | Bishop of point list
  | Queen of point list
  | King of point list

type color = Black | White

type piece = {
  piece_type : general_piece;
  location : point;
  color : color;
  first_move : bool;
}

let get_moves (general_piece : general_piece) =
  match general_piece with
  | Pawn lst | Rook lst | Knight lst | Bishop lst | Queen lst | King lst -> lst