(** Representation of a chess piece. 
    
  Maintains informations related to chess pieces, 
  such as their possible moves, color, location, nad whether it is their first move. *)

type point = int * int
(** abstract type of a point with 2D integer coordinates *)

val pawn : point list
(** [pawn] is valid point list for pawn movement*)

val rook : point list
(** [rook] is valid point list for rook movement*)

val knight : point list
(** [knight] is valid point list for knight movement*)

val bishop : point list
(** [bishop] is valid point list for bishop movement*)

val queen : point list
(** [queen] is valid point list for queen movement*)

val king : point list
(** [king] is valid point list for king movement*)

(** The type of the chess piece. *)
type general_piece =
  | Pawn of point list
  | Rook of point list
  | Knight of point list
  | Bishop of point list
  | Queen of point list
  | King of point list

(** The color of the chess pieces.*)
type color = Black | White

type piece = {
  piece_type : general_piece;
  location : point;
  color : color;
  first_move : bool;
}
(** Abstract data type for a chess piece*)

val get_moves : general_piece -> point list
(** [get_moves general_piece] returns list of points that the [general_piece] can
   move to, given that the current position is (0,0). *)
