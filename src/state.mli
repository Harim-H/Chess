(** Representation of the state of one round of the chess game. *)

open Piece
open Square
open Board

exception UndoFailure
(** [UndoFailure] is an exception raised when there are no previous moves for players to revert.*)

val gen_int : int -> int -> int list
(** [gen_int start finish] creates a list of int from [start] to [finish]
   inclusive. *)

val pawn_locations : color -> point list
(** [pawn_locations color] is a list containing the locations for the pawns based
   on [color]. *)

val pawn_pieces : color -> piece list
(** [pawn_pieces color] is a list containing the pawn pieces based on [color]. *)

val other_piece_locations : color -> point list
(** [other_piece_locations color] is a list containing the locations of chess
   pieces except for pawn based on [color]. *)

val other_piece_type : general_piece list
(** [other_piece_type] is a list containing the general piece representation of
   chess pieces. *)

val other_pieces : color -> piece list
(** [other_pieces color] is a list containing all chess pieces except for pawn
   based on [color]. *)

val generate_pieces : color -> piece list
(** [generate_piece color] creates all pieces of color [color]. *)

val pieces : piece list
(** [pieces] is the list of all pieces, white pieces followed by black pieces. *)

val get_init_board : Square.t Board.t
(** [get_init_board] is the board with initial pieces before game starts. *)

val get_init_anar_board : Square.t Board.t
(** [get_init_anar_board] is the anarchy board with initial pieces before game
   starts *)

val move_piece :
  point * point -> Square.t Board.t list -> bool * Square.t Board.t list
(** [move_piece (start, dest) board] takes in a list of two points, the first
   representing the location for the piece that will be moved to the second
   point, and returns whether the move is successful, and the list of boards,
   with the most recent board on top. *)

val get_first_state : Square.t Board.t list -> Square.t Board.t
(** [get_first_state state_lst] is the first state inside the state list. Raises
   failure if [state_lst] is empty. *)

val revert_state : Square.t Board.t list -> Square.t Board.t list
(** [revert_state state_lst] reverts to the state of the board before the current
   player plays. Raises: UndoFailure if there is not enough state to revert
   to. *)
