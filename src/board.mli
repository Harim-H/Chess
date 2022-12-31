(** Representation of a chess board.

     This module represents a chess board by including a Point module to represent the location of a chess piece.
     Other functions within this module allows access to the pieces on the board and enables checking for other features of the board. *)

open Piece
open Square

(** [Point] is an module to represent the 2D coordinate on a chess board*)
module Point : sig
  type t = int * int
  (** abstract type of a point with 2D integer coordinates *)

  val compare : t -> t -> int
  (** compare two point objects using Stdlib.compare *)
end

module Board : Map.S
(** [Board] is a module created to represent a chess board*)

val init_board : piece list -> Square.t Board.t
(** [init_board pieces] initializes the board by adding object to Board with key
   as piece's location and value being a Square object. *)

val has_piece : point -> Square.t Board.t -> bool
(** [has_piece point board] is whether there is a piece on the square specified
   by [point] on the [board]. *)

val get_piece : point -> Square.t Board.t -> opt_piece
(** [get_piece point board] is the piece on the [board] with location [point].
   None if there is no piece at [point]. *)

val update_piece_loc :
  point -> point -> Square.t Board.t -> opt_piece * Square.t Board.t
(** [update_piece_loc point loc board] is whether capture occurred and the new
   board after moving the piece stored at [point] to [loc], and also updates the
   location of the piece stored at [point]. Returns false and the same board if
   there is no piece at [point]. *)

val get_piece_list : color -> Square.t Board.t -> opt_piece list
(** [get_piece_list color board] is the list of optional pieces on [board] with
   specified [color]. *)

val empty : Square.t Board.t
(** [empty] is an empty board object *)

val add_piece : piece -> Square.t Board.t -> Square.t Board.t
(** [add_piece] adds a single piece to the board based on the piece's location,
   replaces the piece (if any) at that location on the board *)

val add_pieces : piece list -> Square.t Board.t -> Square.t Board.t
(** [add_pieces] adds a list of pieces to the board using [add_piece] *)

val remove_piece : point -> Square.t Board.t -> Square.t Board.t
(** [remove_piece] removes a piece from the board by location. Returns the same
   board if the piece does not exist on the board. *)

val enp_capturing_fal : point -> point -> Square.t Board.t -> bool
(** [enp_capturing_fal start endpoint board] is whether the capturing piece is
   not valid for an enpassant move. Checks whether the piece is a pawn at the
   right row, and the move is valid. *)

val pawn_at_point : point -> Square.t Board.t -> bool
(** [pawn_at_point loc board] is whether there is a pawn at the [loc] specified
   on the [board]. *)

val enp_capturing : piece -> Square.t Board.t -> Square.t Board.t -> bool
(** [enp_capturing captured board] is whether the captured piece satisifed the
   enpassant move requirement. Checks whether the piece is a pawn at two squares
   backward on the [board]. *)

val check_enpassant : point -> point -> Square.t Board.t list -> bool
(** [check_enpassant start endpoint board_lst] is whether an enpassant move can
   be made with the capturing piece moving from [start] to [endpoint] with the
   current board being the top board in [board_lst]. *)

val remove_enp_state : Square.t Board.t list -> Square.t Board.t list
(** [remove_enp_state board_lst] removes the second board in [board_lst] in order
   to set the board ready for undo move. Returns [board_lst] if the length of
   [board_lst] is less than 2. *)

val check_end_piece_ownership : piece -> point -> Square.t Board.t -> bool
(** [check_end_piece_ownership piece end_point board] checks whether or not the
   end piece on [end_point] has the same color as the [piece] being moved.*)
