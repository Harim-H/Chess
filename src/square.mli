(**  Representation of a square on the chess board. 
    
   Used by board to store a piece with a coordinate or no piece at all. 
   Includes a comparison function to satisfy the requirements for the map functor. *)

open Piece

(** abstract type that represents some piece or none. *)
type opt_piece = Some of piece | None

(** Module to represent a square on a board*)
module Square : sig
  type t = { coordinate : int * int; has_piece : opt_piece }
  (** abstract type for square object that has a coordinate and information on
     whether there is a piece there or not *)

  val compare : t -> t -> int
  (** [compare s1 s2] is a standard compare function for squares that uses
     Stdlib.compare *)
end