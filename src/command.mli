(** Parsing of game commands.
    
   This module is responsible for parsing game commands and dealing with checks regarding whether the game moves are valid and legal. *)

open Piece
open Square
open Board

type go_location = piece * string list
(** [go_location] is abstract type of a piece and the moves the piece can take *)

(** [command] is abstract type of user command, either Quit or Go with
   [go_location] *)
type command = Go of go_location | Quit

exception Empty
(** [Empty] is the exception raised when the user command is an empty string*)

exception Malformed
(** [Malformed] is the exception raised when the user command is an unacceptable command*)

val has_piece_in_path : point -> point -> Square.t Board.t -> point -> bool
(** [has_piece_in_path] is whether there is a piece on [board] between
   [start_point] and [end_point], with each step on the path to [end_point] by
   [direction]. Returns false if [start_point] and [end_point] are the same. *)

val illegal_jump : bool -> piece -> point -> Square.t Board.t -> bool
(** [illegal_jump capturing piece end_point board] is whether there is a illegal
   jump when [piece] moves from its current location to [end_point] by checking
   whether there are pieces in between on the [board]. Always false for Knight
   and King pieces. Accounts for capturing of a piece by not checking whether
   there is a piece at the [end_point]. TODO: add castling or pawn double
   forward. *)

val is_move_available : bool -> piece -> point -> bool
(** [is_move_available capturing piece end_point] is whether the move is 
    available in the list of possible moves for the specific [piece]. 
    For pawn, checks for whether the move is in the list of possible moves, 
    a two step move as a first move, or whether or not there is a piece at 
    [end_point] for a diagonal move. 
    For other piece types, checks whether the move is available or not. *)

val is_move_beyond_boundary : point -> bool
(** [is_move_beyond_boundary end_point] is whether the move end_point is beyond the boundary of a chess board. *)

val is_legal_move : bool -> piece -> point -> Square.t Board.t -> bool
(** [is_legal_move capturing piece end_point board] is whether the attempt for
   [piece] to change its location to [point] is legal by chess rules. For pawn,
   checks for whether the move is in the list of possible moves, a two step move
   as a first move, or whether or not there is a piece at [end_point] for a
   diagonal move. Checks whether the move is available for other piece types.
   Additionally checks for whether the piece will go outside the board's
   boundary and illegal jump for all piece types. *)

val split_str : string -> string list
(** [split_str str] is the result of parsing the user command [str] into a list
   of strings. For command to move a piece, parse the string a tuple with the
   piece type to move and the location to which the piece should move to. For a
   capture command, same parsing of the string with additional capturing keyword
   as the head of the list. *)

val end_coord : string -> point
(** [end_coord str] is a point that results from parsing [str], which represents
   a coordinate in chess notation. Raises: Malformed when x coordinate is not
   valid. Failwith error if [str] does not have a length of 2 or y coordinate is
   not between 1 and 9. *)

val piece_code_to_general : string -> general_piece
(** [piece_code_to_general piece_code] is a general_piece that is represented by
   [piece_code], which is in chess notation. Raises: Malformed if [piece_code]
   is not valid. Example: "Ke" -> King king. "" -> Pawn (pawn first letter or
   empty). *)

val list_general_piece : general_piece -> piece list -> piece list -> piece list
(** [list_general_piece p_type pieces acc] is the list of all pieces in [pieces]
   that have piece_type [p_type]. Uses [acc] to achieve tail recursion. *)

val list_potential_pieces :
  bool -> point -> piece list -> piece list -> Square.t Board.t -> piece list
(** [list_potential_pieces capturing endpoint pieces acc board] is the list of
   pieces on [board] that can go to [endpoint]. Uses [acc] to achieve tail
   recursion and [pieces] to contain the current pieces on the [board]. *)

val spec_coord_noncap : string list -> int * int
(** [spec_coord_noncap input] is the x and y coordinates specified by [input]
   command for noncapturing commands. Example: when input = Re, specific_x_coord
   returns 5 as e represents the x coordinate. *)

val spec_coord_cap : string list -> int * int
(** [spec_coord_cap input] is the x and y coordinates specified by [input]
   command for capturing commands. Example: when input = Re, specific_x_coord
   returns 5 as e represents the x coordinate. *)

val matching_coord_piece :
  piece list -> (int * int -> int) -> int -> piece list -> piece list
(** [matching_coord_piece lst x] is the first piece in [lst] with x coordinate
   [x]. Raises: failwith if there is no such piece. *)

val specific_piece : string list -> piece list -> Square.t Board.t -> piece
(** [specific_piece input pieces_list board] is the specific piece that can be
   moved given user input string list [input] and list of pieces the player has
   in [pieces_list], and uses [board] to search through the available pieces on
   the board. Raises: failwith if no piece can move to the location specified by
   user command. *)

val start_end_coord : string -> piece list -> Square.t Board.t -> point list
(** [start_end_coord input pieces_list] is a tuple of the start and end
   coordinates of a move, using [input], [board] and the list of pieces that the
   player has in [piece_list]. *)

val input_enpassant : string -> Square.t Board.t -> color -> bool
(** [input_enpassant input board] is whether the [input] can indicate an
   enpassant move, that resembles a capture move where the destination has no
   piece. *)

val enpassant_points : string -> color -> point * point
(** [enpssant_points input color] returns a tuple containing the start and
   endpoints of the capturing piece for an enpassant move. *)

val get_king : Square.t Board.t -> color -> piece
(** [get_king st turn] returns the king of the chosen color [turn] on the board
   [st]*)

val list_of_attackers : Square.t Board.t -> color -> piece list
(** [list_of_attackers st turn] returns the list of pieces that are attacking the
   king of the color [turn]*)

val is_in_check : Square.t Board.t -> color -> bool
(** [is_in_check st turn] returns whether the chosen side [turn] is in check or
   not in the given position [st]*)

val move_is_check : Square.t Board.t -> color -> piece list -> point -> bool
(** [move_is_check st turn pieces move] returns whether moving a piece will result in check in the next move *)

val has_no_legal_move : Square.t Board.t -> color -> bool
(** [has_no_legal_move st turn] returns whether the chosen side has a legal
   move*)

val insufficient_material : Square.t Board.t -> bool
(** [insufficient_material st] returns whether there is not sufficient material
   to checkmate*)

val shortcastle : Square.t Board.t list -> color -> bool * Square.t Board.t list
(** [shortcastle st turn] returns a tuple of true and the position of the king
   castled short if the chosen side can castle *)

val longcastle : Square.t Board.t list -> color -> bool * Square.t Board.t list
(** [shortcastle st turn] returns a tuple of true and the position of the king
   castled long if the chosen side can castle *)

val castle :
  string -> Square.t Board.t list -> color -> bool * Square.t Board.t list
(** [castle st turn] takes a string containing a hyphen and determines whether
   the string is to castle long or short; this is then passed through longcastle
   or shortcastle*)

val promote :
  Square.t Board.t list -> point -> color -> string -> Square.t Board.t list
(** [promote st pt input turn] takes a state that needs to a pawn promotion and
   its input to determine the promotion*)
