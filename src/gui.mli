(** Helper functions to deal with Gui display.  *)

val piece_type_color_to_string : Square.opt_piece -> string
(** returns: Unicode representation of chess pieces in String *)

val print_board : Square.Square.t Board.Board.t -> string
(** returns: String Representation of a valid chess board *)
