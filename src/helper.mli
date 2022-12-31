(** Helper functions for other features within the game.  *)

open Piece
open Square

val rowGen : int -> (int * int) list
(** [rowGen] returns the list of points that represents valid chess board cordinate on
   destinated rows *)

val letter_to_number : char -> int
(** [letter_to_number] convert charcter to number for the internal cordinate system *)

val not_color : color -> color
(** [not_color color] is an negation operation on color. If [Color] = White,
  returns Black. If [Color] = Black, returns White *)

val opt_piece_to_piece : opt_piece -> piece
(** [opt_piece_to_piece opt] converts opt_piece to piece *)

val unopt_pieces : opt_piece list -> piece list
(** [unopt_pieces] returns a normal piece list from an opt_piece list *)
