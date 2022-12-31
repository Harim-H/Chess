open OUnit2
open Game
open Piece
open Square
open Board
open State
open Command
open Helper

(* Test plan: The OUnit tests were initially written using black-box testing so
   as not to be influenced by the implementation but rather focus on
   functionality from the client's perspective. Afterwards, more tests were
   added using glass-box testing to ensure correctness of edge cases. From this
   combination of black and glass box OUnit testing, bisect reports 80+% test
   coverage. This high percentage, combined with extensive play testing,
   demonstrates correctness of the system.

   The following were tested using OUnit:

   - proper board setup with Board, Point, and Square modules

   - standard piece movements and capturing

   - special cases/features (ex: en passant, castling, check, promotions, undo)

   The following were manually tested by playing the game:

   - edge cases (ex: checkmate, proper castling)

   - game features (ex: "anarchy mode", text and visual output)

   - overall playability (ex: full games from start to finish, including
   features tested with OUnit) *)

(*misc helpers*)
let get_piece pts board = opt_piece_to_piece (get_piece pts board)

let compare_boards (b1 : Square.t Board.t) (b2 : Square.t Board.t) =
  let b1_pieces =
    get_piece_list White b1 @ get_piece_list Black b1 |> unopt_pieces
  in
  let b2_pieces =
    get_piece_list White b2 @ get_piece_list Black b2 |> unopt_pieces
  in
  List.length b1_pieces = List.length b2_pieces

(*method to move one piece multiple times*)
let rec multi_input_helper (lst : string list) (pieces : piece list)
    (expected_output : ((int * int) * (int * int)) list)
    (board : Square.t Board.t) =
  match lst with
  | [] -> List.length expected_output = 0
  | h :: t -> (
      match expected_output with
      | [] -> failwith "expected output list shorter than input list"
      | eh :: et ->
          assert ([ fst eh; snd eh ] = start_end_coord h pieces board);
          multi_input_helper t pieces et board)

let start_pieces = unopt_pieces (get_piece_list White get_init_board)

(*Initializing piece for promotion testing*)
let p =
  {
    piece_type = Queen queen;
    location = (1, 8);
    color = White;
    first_move = false;
  }

(*Initizalizing pieces for castling testing*)
let expected_long_castle_pieces =
  [
    {
      piece_type = King king;
      location = (3, 1);
      color = White;
      first_move = false;
    };
    {
      piece_type = Rook rook;
      location = (4, 1);
      color = White;
      first_move = false;
    };
  ]

let expected_short_castle_pieces =
  [
    {
      piece_type = King king;
      location = (7, 8);
      color = Black;
      first_move = false;
    };
    {
      piece_type = Rook rook;
      location = (6, 8);
      color = Black;
      first_move = false;
    };
  ]

(*test function definitions*********************************)
let illegal_jump_test (name : string) (capturing : bool) (piece : piece)
    (end_point : point) (board : Square.t Board.t) (expected_ouput : bool) =
  name >:: fun _ ->
  assert_equal expected_ouput (illegal_jump capturing piece end_point board)

let promote_test (name : string) (st : Square.t Board.t list) (pt : point)
    (turn : color) (input : string) expected_output =
  name >:: fun _ ->
  let b = promote st pt turn input in
  ignore b;
  assert_equal expected_output (get_piece pt (List.hd b))

let promote_fail_test (name : string) (st : Square.t Board.t list) (pt : point)
    (turn : color) (input : string) expected_output =
  name >:: fun _ ->
  let b () = promote st pt turn input in
  assert_raises expected_output b

let castle_test (name : string) (input : string)
    (board_lst : Square.t Board.t list) (turn : color) king_pos rook_pos
    expected_output =
  let b = castle input board_lst turn in
  ignore b;
  name >:: fun _ ->
  assert_equal expected_output
    [
      get_piece king_pos (List.hd (snd b)); get_piece rook_pos (List.hd (snd b));
    ]

let castle_fail_test (name : string) (input : string)
    (board_lst : Square.t Board.t list) (turn : color) expected_output =
  name >:: fun _ ->
  let b () = castle input board_lst turn in
  assert_raises expected_output b

let insufficient_material_test (name : string) (board : Square.t Board.t)
    expected_output =
  name >:: fun _ -> assert_equal expected_output (insufficient_material board)

let has_no_legal_move_test (name : string) (board : Square.t Board.t)
    (turn : color) expected_output =
  name >:: fun _ -> assert_equal expected_output (has_no_legal_move board turn)

let start_end_coord_test (name : string) (input : string) (pieces : piece list)
    (expected_output : (int * int) list) (board : Square.t Board.t) : test =
  name >:: fun _ ->
  assert_equal expected_output (start_end_coord input pieces board)

let exception_start_end_coord_test (name : string) (input : string)
    (pieces : piece list) (expected_output : exn) (board : Square.t Board.t) :
    test =
  name >:: fun _ ->
  assert_raises expected_output (fun () -> start_end_coord input pieces board)

let into_check_test (name : string) (input : string) (pieces : piece list)
    (expected_output : bool) (board : Square.t Board.t) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (move_is_check board (List.hd pieces).color pieces
       (List.hd (start_end_coord input pieces board)))

(*test multiple moves of one piece*)
let specific_piece_moves_test (name : string) (input : string list)
    (pieces : piece list) (expected_output : ((int * int) * (int * int)) list)
    (board : Square.t Board.t) : test =
  name >:: fun _ ->
  assert (multi_input_helper input pieces expected_output board)

let check_opponent_test (name : string) (input : string) (pieces : piece list)
    (expected_output : bool) (board : Square.t Board.t) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (is_in_check
       (update_piece_loc
          (List.hd (start_end_coord input pieces board))
          (List.nth (start_end_coord input pieces board) 1)
          board
       |> snd)
       (match (List.hd pieces).color with
       | White -> Black
       | Black -> White))

let board_test (name : string) (b1 : Square.t Board.t) (b2 : Square.t Board.t) =
  name >:: fun _ -> assert_equal true (compare_boards b1 b2)

let exception_test (name : string) (boards : Square.t Board.t list) =
  name >:: fun _ -> assert_raises UndoFailure (fun () -> revert_state boards)

let check_enp_test (name : string) (start : point) (endpoint : point)
    (board_lst : Square.t Board.t list) (expected_ouput : bool) =
  name >:: fun _ ->
  assert_equal expected_ouput (check_enpassant start endpoint board_lst)

let remove_enp_state_test (name : string) (input : Square.t Board.t list)
    (expected_ouput : Square.t Board.t list) =
  name >:: fun _ -> assert_equal expected_ouput (remove_enp_state input)

(*board configurations*********************************************)

let init_board = get_init_board

let illegal_jump_has_piece =
  add_pieces
    [
      {
        piece_type = Pawn pawn;
        location = (1, 1);
        color = White;
        first_move = false;
      };
      {
        piece_type = Knight knight;
        location = (1, 2);
        color = Black;
        first_move = false;
      };
    ]
    Board.empty

let promotion_board =
  add_pieces
    [
      {
        piece_type = Pawn pawn;
        location = (1, 8);
        color = White;
        first_move = true;
      };
    ]
    Board.empty

let long_castling_board =
  add_pieces
    [
      {
        piece_type = King king;
        location = (5, 1);
        color = White;
        first_move = true;
      };
      {
        piece_type = Rook rook;
        location = (1, 1);
        color = White;
        first_move = true;
      };
    ]
    Board.empty

let long_castling_rook_moved_board =
  add_pieces
    [
      {
        piece_type = King king;
        location = (5, 1);
        color = White;
        first_move = true;
      };
      {
        piece_type = Rook rook;
        location = (1, 1);
        color = White;
        first_move = false;
      };
    ]
    Board.empty

let long_castling_with_queen_in_path_board =
  add_pieces
    [
      {
        piece_type = King king;
        location = (5, 1);
        color = White;
        first_move = true;
      };
      {
        piece_type = Queen queen;
        location = (4, 1);
        color = White;
        first_move = true;
      };
      {
        piece_type = Rook rook;
        location = (1, 1);
        color = White;
        first_move = true;
      };
    ]
    Board.empty

let castling_first_move_false_fail_board =
  add_pieces
    [
      {
        piece_type = King king;
        location = (5, 1);
        color = White;
        first_move = false;
      };
      {
        piece_type = Rook rook;
        location = (8, 1);
        color = White;
        first_move = true;
      };
    ]
    Board.empty

let short_castling_board =
  add_pieces
    [
      {
        piece_type = King king;
        location = (5, 8);
        color = Black;
        first_move = true;
      };
      {
        piece_type = Rook rook;
        location = (8, 8);
        color = Black;
        first_move = true;
      };
    ]
    Board.empty

let short_castling_in_check_board =
  add_pieces
    [
      {
        piece_type = King king;
        location = (5, 1);
        color = White;
        first_move = true;
      };
      {
        piece_type = Rook rook;
        location = (8, 1);
        color = White;
        first_move = true;
      };
      {
        piece_type = Rook rook;
        location = (1, 1);
        color = Black;
        first_move = false;
      };
    ]
    Board.empty

let short_castling_rook_moved_board =
  add_pieces
    [
      {
        piece_type = King king;
        location = (5, 1);
        color = White;
        first_move = true;
      };
      {
        piece_type = Rook rook;
        location = (8, 1);
        color = White;
        first_move = false;
      };
    ]
    Board.empty

let short_castling_with_knight_in_path_board =
  add_pieces
    [
      {
        piece_type = King king;
        location = (5, 1);
        color = White;
        first_move = true;
      };
      {
        piece_type = Knight knight;
        location = (7, 1);
        color = White;
        first_move = true;
      };
      {
        piece_type = Rook rook;
        location = (8, 1);
        color = White;
        first_move = true;
      };
    ]
    Board.empty

let single_pawn_board =
  add_pieces
    [
      {
        piece_type = Pawn pawn;
        location = (1, 2);
        color = White;
        first_move = true;
      };
    ]
    Board.empty

let board_bishop_capturing =
  add_pieces
    [
      {
        piece_type = Pawn pawn;
        location = (1, 7);
        color = Black;
        first_move = true;
      };
      {
        piece_type = Rook rook;
        location = (8, 8);
        color = Black;
        first_move = true;
      };
      {
        piece_type = Bishop bishop;
        location = (5, 3);
        color = Black;
        first_move = false;
      };
      {
        piece_type = Queen queen;
        location = (2, 2);
        color = Black;
        first_move = false;
      };
      {
        piece_type = Bishop bishop;
        location = (4, 4);
        color = White;
        first_move = false;
      };
    ]
    Board.empty

let board_bishop_capturing_pieces =
  unopt_pieces (get_piece_list White board_bishop_capturing)

let board_knight_capturing =
  add_pieces
    [
      {
        piece_type = Knight knight;
        location = (4, 5);
        color = White;
        first_move = false;
      };
      {
        piece_type = Knight knight;
        location = (4, 1);
        color = White;
        first_move = false;
      };
      {
        piece_type = Knight knight;
        location = (8, 7);
        color = White;
        first_move = false;
      };
      {
        piece_type = Knight knight;
        location = (1, 8);
        color = Black;
        first_move = false;
      };
      {
        piece_type = Bishop bishop;
        location = (5, 3);
        color = Black;
        first_move = false;
      };
      {
        piece_type = Queen queen;
        location = (3, 7);
        color = Black;
        first_move = false;
      };
      {
        piece_type = Knight knight;
        location = (5, 7);
        color = Black;
        first_move = false;
      };
      {
        piece_type = Pawn pawn;
        location = (2, 6);
        color = Black;
        first_move = false;
      };
    ]
    Board.empty

let board_knight_capturing_pieces =
  unopt_pieces (get_piece_list White board_knight_capturing)

let board_rook_capturing =
  add_pieces
    [
      {
        piece_type = Pawn pawn;
        location = (7, 5);
        color = Black;
        first_move = false;
      };
      {
        piece_type = Bishop bishop;
        location = (6, 8);
        color = Black;
        first_move = false;
      };
      {
        piece_type = Queen queen;
        location = (1, 5);
        color = Black;
        first_move = false;
      };
      {
        piece_type = Knight knight;
        location = (6, 3);
        color = Black;
        first_move = false;
      };
      {
        piece_type = Rook rook;
        location = (6, 5);
        color = White;
        first_move = false;
      };
      {
        piece_type = Rook rook;
        location = (3, 7);
        color = White;
        first_move = false;
      };
      {
        piece_type = Rook rook;
        location = (6, 1);
        color = White;
        first_move = false;
      };
      {
        piece_type = Rook rook;
        location = (7, 6);
        color = White;
        first_move = false;
      };
      {
        piece_type = Queen queen;
        location = (4, 3);
        color = White;
        first_move = false;
      };
    ]
    Board.empty

let board_rook_capturing_pieces =
  unopt_pieces (get_piece_list White board_rook_capturing)

let board_pawn_capturing =
  add_pieces
    [
      {
        piece_type = Pawn pawn;
        location = (1, 2);
        color = White;
        first_move = true;
      };
      {
        piece_type = Pawn pawn;
        location = (3, 2);
        color = White;
        first_move = true;
      };
      {
        piece_type = Pawn pawn;
        location = (6, 4);
        color = White;
        first_move = false;
      };
      {
        piece_type = Pawn pawn;
        location = (7, 5);
        color = Black;
        first_move = false (*moved up 2 previous turn*);
      };
      {
        piece_type = Pawn pawn;
        location = (8, 5);
        color = White;
        first_move = false;
      };
      {
        piece_type = Bishop bishop;
        location = (4, 3);
        color = Black;
        first_move = false;
      };
      {
        piece_type = Rook rook;
        location = (2, 3);
        color = Black;
        first_move = false;
      };
      {
        piece_type = Queen queen;
        location = (5, 5);
        color = Black;
        first_move = false;
      };
      {
        piece_type = Pawn pawn;
        location = (1, 5);
        color = White;
        first_move = false;
      };
      {
        piece_type = Pawn pawn;
        location = (2, 5);
        color = Black;
        first_move = false (*moved up 2 previous turn*);
      };
    ]
    Board.empty

let board_pawn_capturing_pieces =
  unopt_pieces (get_piece_list White board_pawn_capturing)

let board_pawn_special =
  add_pieces
    [
      {
        piece_type = Pawn pawn;
        location = (1, 6);
        color = White;
        first_move = false;
      };
      {
        piece_type = Pawn pawn;
        location = (1, 7);
        color = Black;
        first_move = true;
      };
      {
        piece_type = Pawn pawn;
        location = (2, 6);
        color = Black;
        first_move = false;
      };
      {
        piece_type = Pawn pawn;
        location = (2, 2);
        color = White;
        first_move = true;
      };
      {
        piece_type = Knight knight;
        location = (2, 3);
        color = Black;
        first_move = false;
      };
      {
        piece_type = Rook rook;
        location = (3, 5);
        color = Black;
        first_move = false;
      };
      {
        piece_type = Pawn pawn;
        location = (4, 5);
        color = White;
        first_move = false;
      };
      {
        piece_type = Pawn pawn;
        location = (5, 5);
        color = Black;
        first_move = false (*moved 2 but not in the previous turn*);
      };
      {
        piece_type = Pawn pawn;
        location = (5, 7);
        color = White;
        first_move = false;
      };
      {
        piece_type = Bishop bishop;
        location = (5, 8);
        color = Black;
        first_move = false;
      };
      {
        piece_type = Pawn pawn;
        location = (6, 7);
        color = White;
        first_move = false;
      };
      {
        piece_type = Pawn pawn;
        location = (6, 2);
        color = White;
        first_move = true;
      };
      {
        piece_type = Pawn pawn;
        location = (6, 3);
        color = Black;
        first_move = false;
      };
      {
        piece_type = Pawn pawn;
        location = (7, 5);
        color = White;
        first_move = false;
      };
      {
        piece_type = Pawn pawn;
        location = (8, 5);
        color = Black;
        first_move = false;
      };
      {
        piece_type = Pawn pawn;
        location = (8, 7);
        color = White;
        first_move = false;
      };
    ]
    Board.empty

let board_pawn_special_pieces =
  unopt_pieces (get_piece_list White board_pawn_special)

let board_into_check_illegal =
  add_pieces
    [
      {
        piece_type = King king;
        location = (2, 6);
        color = White;
        first_move = false;
      };
      {
        piece_type = Knight knight;
        location = (2, 8);
        color = Black;
        first_move = false;
      };
      {
        piece_type = Rook rook;
        location = (1, 8);
        color = Black;
        first_move = false;
      };
      {
        piece_type = Bishop bishop;
        location = (3, 8);
        color = Black;
        first_move = false;
      };
      {
        piece_type = Bishop bishop;
        location = (6, 8);
        color = Black;
        first_move = false;
      };
      {
        piece_type = Pawn pawn;
        location = (1, 7);
        color = Black;
        first_move = false;
      };
    ]
    Board.empty

let board_into_check_illegal_pieces =
  unopt_pieces (get_piece_list White board_into_check_illegal)

let board_check_opponent =
  add_pieces
    [
      {
        piece_type = King king;
        location = (4, 5);
        color = Black;
        first_move = false;
      };
      {
        piece_type = Queen queen;
        location = (5, 7);
        color = White;
        first_move = false;
      };
      {
        piece_type = Rook rook;
        location = (1, 6);
        color = White;
        first_move = false;
      };
      {
        piece_type = Knight knight;
        location = (7, 4);
        color = White;
        first_move = false;
      };
      {
        piece_type = Rook rook;
        location = (1, 4);
        color = White;
        first_move = false;
      };
      {
        piece_type = Pawn pawn;
        location = (5, 3);
        color = Black;
        first_move = false;
      };
    ]
    Board.empty

let board_check_opponent_pieces =
  unopt_pieces (get_piece_list White board_check_opponent)

let board_castling_illegal_1 =
  add_pieces
    [
      {
        piece_type = King king;
        location = (5, 1);
        color = White;
        first_move = false;
      };
      {
        piece_type = Rook rook;
        location = (8, 1);
        color = White;
        first_move = true;
      };
      {
        piece_type = Rook rook;
        location = (1, 1);
        color = White;
        first_move = true;
      };
    ]
    Board.empty

let board_castling_illegal_1_pieces =
  unopt_pieces (get_piece_list White board_castling_illegal_1)

let board_castling_illegal_2 =
  add_pieces
    [
      {
        piece_type = King king;
        location = (5, 1);
        color = White;
        first_move = true;
      };
      {
        piece_type = Rook rook;
        location = (8, 1);
        color = White;
        first_move = false;
      };
      {
        piece_type = Rook rook;
        location = (1, 1);
        color = Black;
        first_move = true;
      };
    ]
    Board.empty

let board_castling_illegal_2_pieces =
  unopt_pieces (get_piece_list White board_castling_illegal_2)

let board_castling_illegal_3 =
  add_pieces
    [
      {
        piece_type = King king;
        location = (5, 1);
        color = White;
        first_move = true;
      };
      {
        piece_type = Rook rook;
        location = (7, 1);
        color = White;
        first_move = true;
      };
      {
        piece_type = Knight knight;
        location = (1, 1);
        color = White;
        first_move = true;
      };
    ]
    Board.empty

let board_castling_illegal_3_pieces =
  unopt_pieces (get_piece_list White board_castling_illegal_3)

let board_castling_illegal_4 =
  add_pieces
    [
      {
        piece_type = King king;
        location = (5, 1);
        color = White;
        first_move = true;
      };
      {
        piece_type = Rook rook;
        location = (8, 1);
        color = White;
        first_move = true;
      };
      {
        piece_type = Rook rook;
        location = (1, 1);
        color = White;
        first_move = true;
      };
      {
        piece_type = Bishop bishop;
        location = (6, 1);
        color = White;
        first_move = true;
      };
      {
        piece_type = Bishop bishop;
        location = (1, 4);
        color = Black;
        first_move = true;
      };
    ]
    Board.empty

let board_castling_illegal_4_pieces =
  unopt_pieces (get_piece_list White board_castling_illegal_4)

let board_castling_illegal_5 =
  add_pieces
    [
      {
        piece_type = King king;
        location = (5, 1);
        color = White;
        first_move = true;
      };
      {
        piece_type = Rook rook;
        location = (8, 1);
        color = White;
        first_move = true;
      };
      {
        piece_type = Rook rook;
        location = (1, 1);
        color = White;
        first_move = true;
      };
      {
        piece_type = Bishop bishop;
        location = (2, 4);
        color = Black;
        first_move = true;
      };
    ]
    Board.empty

let board_castling_illegal_5_pieces =
  unopt_pieces (get_piece_list White board_castling_illegal_5)

let board_castling_illegal_6 =
  add_pieces
    [
      {
        piece_type = King king;
        location = (5, 1);
        color = White;
        first_move = true;
      };
      {
        piece_type = Rook rook;
        location = (8, 1);
        color = White;
        first_move = true;
      };
      {
        piece_type = Rook rook;
        location = (1, 1);
        color = White;
        first_move = true;
      };
      {
        piece_type = Bishop bishop;
        location = (4, 4);
        color = Black;
        first_move = true;
      };
    ]
    Board.empty

let board_castling_illegal_6_pieces =
  unopt_pieces (get_piece_list White board_castling_illegal_6)

let pawn_moves =
  move_piece ((2, 2), (2, 4)) [ init_board ]
  |> snd
  |> move_piece ((4, 7), (4, 6))
  |> snd

let enp_captured_move =
  move_piece ((2, 4), (2, 5)) pawn_moves
  |> snd
  |> move_piece ((1, 7), (1, 5))
  |> snd

let past_enp_capture =
  move_piece ((3, 2), (3, 4)) enp_captured_move
  |> snd
  |> move_piece ((8, 7), (8, 5))
  |> snd

(*test lists, grouped by category**************************************)
let into_check_illegal_tests =
  [
    into_check_test "Can't move into check Kb7" "Kb7"
      board_into_check_illegal_pieces true board_into_check_illegal;
    exception_start_end_coord_test "Can't move into check Ka7" "Ka7"
      board_into_check_illegal_pieces (Failure "no pieces can move here")
      board_into_check_illegal;
    into_check_test "Can't move into\n       check Kc6" "Kc6"
      board_into_check_illegal_pieces true board_into_check_illegal;
    into_check_test "Can't move into check Ka6" "Ka6"
      board_into_check_illegal_pieces true board_into_check_illegal;
  ]

let check_opponent_tests =
  [
    check_opponent_test "put black into check Qe6" "Qe6"
      board_check_opponent_pieces true board_check_opponent;
    check_opponent_test "put black into check Nxe3" "Nxe3"
      board_check_opponent_pieces true board_check_opponent;
    check_opponent_test "doesn't put black into chec, aka legal" "Qe8"
      board_check_opponent_pieces false board_check_opponent;
  ]

let special_pawns_tests =
  [
    (*pawn capturing diagonal*)
    start_end_coord_test
      "pawn right diagonal capture first move, 2 pawn choices" "axb3"
      board_pawn_capturing_pieces
      [ (1, 2); (2, 3) ]
      board_pawn_capturing;
    start_end_coord_test
      "pawn left diagonal capture first move, 2 pawn/capture\n  choices" "cxb3"
      board_pawn_capturing_pieces
      [ (3, 2); (2, 3) ]
      board_pawn_capturing;
    start_end_coord_test
      "pawn right diagonal capture first\n  move, 2 capture choices" "cxd3"
      board_pawn_capturing_pieces
      [ (3, 2); (4, 3) ]
      board_pawn_capturing;
    start_end_coord_test "pawn left diagonal capture, 2\n  capture choices"
      "fxe5" board_pawn_capturing_pieces
      [ (6, 4); (5, 5) ]
      board_pawn_capturing;
    start_end_coord_test "pawn right diagonal capture, 2\n  capture choices"
      "fxg5" board_pawn_capturing_pieces
      [ (6, 4); (7, 5) ]
      board_pawn_capturing;
    (*illegal en passant*)
    exception_start_end_coord_test "pawn can't enpassant when opponent moves 1"
      "axb7" board_pawn_special_pieces (Failure "no pieces can move here")
      board_pawn_special;
    exception_start_end_coord_test "pawn can't en passant\n  non-pawn" "dxc6"
      board_pawn_special_pieces (Failure "no pieces can move here")
      board_pawn_special;
    exception_start_end_coord_test
      "pawn can't en passant when opponent moves 2 but not in last turn" "dxe6"
      board_pawn_special_pieces (Failure "no pieces can move here")
      board_pawn_special;
    exception_start_end_coord_test
      "pawn can't en passant when opponent moves 1 twice" "gxh6"
      board_pawn_special_pieces (Failure "no pieces can move here")
      board_pawn_special;
    exception_start_end_coord_test "pawn can't en passant on\n  first move"
      "fxg3" board_pawn_special_pieces (Failure "no pieces can move here")
      board_pawn_special;
    (*other illegal pawn moves*)
    exception_start_end_coord_test "pawn can't forward capture" "fxf3"
      board_pawn_special_pieces (Failure "no pieces can move here")
      board_pawn_special;
    exception_start_end_coord_test
      "pawn can't move forward\n  with piece in the way" "e8"
      board_pawn_special_pieces (Failure "no pieces can move here")
      board_pawn_special;
    exception_start_end_coord_test "pawn can't\n  jump" "a8"
      board_pawn_special_pieces (Failure "no pieces can move here")
      board_pawn_special;
    exception_start_end_coord_test "pawn can't jump on first\n  move" "b4"
      board_pawn_special_pieces (Failure "no pieces can move here")
      board_pawn_special;
  ]

(*test all white pawns moving up 1 and 2 steps*)
let all_white_pawns_tests =
  [
    start_end_coord_test "a3: (1,2) -> (1,3) with board initial" "a3"
      start_pieces
      [ (1, 2); (1, 3) ]
      single_pawn_board;
    (*testing custom board too*)
    start_end_coord_test "b3: (2,2) -> (2,3) with board initial" "b3"
      start_pieces
      [ (2, 2); (2, 3) ]
      init_board;
    start_end_coord_test "c3: (3,2) -> (3,3) with\n  board initial" "c3"
      start_pieces
      [ (3, 2); (3, 3) ]
      init_board;
    start_end_coord_test "d3: (4,2) -> (4,3) with board initial" "d3"
      start_pieces
      [ (4, 2); (4, 3) ]
      init_board;
    start_end_coord_test "e3: (5,2) -> (5,3) with\n  board initial" "e3"
      start_pieces
      [ (5, 2); (5, 3) ]
      init_board;
    start_end_coord_test "f3: (6,2) -> (6,3) with board initial" "f3"
      start_pieces
      [ (6, 2); (6, 3) ]
      init_board;
    start_end_coord_test "g3: (7,2) -> (7,3) with\n  board initial" "g3"
      start_pieces
      [ (7, 2); (7, 3) ]
      init_board;
    start_end_coord_test "h3: (8,2) -> (8,3) with board initial" "h3"
      start_pieces
      [ (8, 2); (8, 3) ]
      init_board;
    start_end_coord_test "a4: (1,2) -> (1,4) with\n  board initial" "a4"
      start_pieces
      [ (1, 2); (1, 4) ]
      init_board;
    start_end_coord_test "b4: (2,2) -> (2,4) with board initial" "b4"
      start_pieces
      [ (2, 2); (2, 4) ]
      init_board;
    start_end_coord_test "c4: (3,2) -> (3,4) with\n  board initial" "c4"
      start_pieces
      [ (3, 2); (3, 4) ]
      init_board;
    start_end_coord_test "d4: (4,2) -> (4,4) with board initial" "d4"
      start_pieces
      [ (4, 2); (4, 4) ]
      init_board;
    start_end_coord_test "e4: (5,2) -> (5,4) with\n  board initial" "e4"
      start_pieces
      [ (5, 2); (5, 4) ]
      init_board;
    start_end_coord_test "f4: (6,2) -> (6,4) with board initial" "f4"
      start_pieces
      [ (6, 2); (6, 4) ]
      init_board;
    start_end_coord_test "g4: (7,2) -> (7,4) with\n  board initial" "g4"
      start_pieces
      [ (7, 2); (7, 4) ]
      init_board;
    start_end_coord_test "h4: (8,2) -> (8,4) with board initial" "h4"
      start_pieces
      [ (8, 2); (8, 4) ]
      init_board;
  ]

let castling_tests =
  [
    (*invalid castling*)
    exception_start_end_coord_test "Can't castle when not king first move"
      "O-O-O" board_castling_illegal_1_pieces
      (Failure "coordinate need to be be exactly 2 chars")
      board_castling_illegal_1;
    exception_start_end_coord_test
      "Can't\n      castle when not king first move" "0-0"
      board_castling_illegal_1_pieces
      (Failure "coordinate need to be be exactly 2 chars")
      board_castling_illegal_1;
    exception_start_end_coord_test "Can't castle when not rook first move" "O-O"
      board_castling_illegal_2_pieces
      (Failure "coordinate need to be be exactly 2 chars")
      board_castling_illegal_2;
    exception_start_end_coord_test "Can't\n      castle with opposite color"
      "O-O-O" board_castling_illegal_2_pieces
      (Failure "coordinate need to be be exactly 2 chars")
      board_castling_illegal_2;
    exception_start_end_coord_test "Can't castle with opposite color" "O-O-O"
      board_castling_illegal_2_pieces
      (Failure "coordinate need to be be exactly 2 chars")
      board_castling_illegal_2;
    exception_start_end_coord_test "Can't\n      castle with non rook" "O-O-O"
      board_castling_illegal_3_pieces
      (Failure "coordinate need to be be exactly 2 chars")
      board_castling_illegal_3;
    exception_start_end_coord_test "Can't castle from non original location"
      "O-O" board_castling_illegal_3_pieces
      (Failure "coordinate need to be be exactly 2 chars")
      board_castling_illegal_3;
    exception_start_end_coord_test "Squares\n      occupied" "O-O"
      board_castling_illegal_4_pieces
      (Failure "coordinate need to be be exactly 2 chars")
      board_castling_illegal_4;
    exception_start_end_coord_test "Can't castle when moves over spot in check"
      "O-O-O" board_castling_illegal_4_pieces
      (Failure "coordinate need to be be exactly 2 chars")
      board_castling_illegal_4;
    exception_start_end_coord_test "Can't castle\n      when in check" "O-O-O"
      board_castling_illegal_5_pieces
      (Failure "coordinate need to be be exactly 2 chars")
      board_castling_illegal_5;
    exception_start_end_coord_test "Can't castle into check" "O-O"
      board_castling_illegal_6_pieces
      (Failure "coordinate need to be be exactly 2 chars")
      board_castling_illegal_6;
    castle_test "long castling test" "O-O-O" [ long_castling_board ] White
      (3, 1) (4, 1) expected_long_castle_pieces;
    castle_test "short castling test" "0-0" [ short_castling_board ] Black
      (7, 8) (6, 8) expected_short_castle_pieces;
    castle_test "short castling test" "O-O" [ short_castling_board ] Black
      (7, 8) (6, 8) expected_short_castle_pieces;
    castle_fail_test "short castling with false first move test" "O-O"
      [ castling_first_move_false_fail_board ]
      White (Failure "Can't castle after first move");
    castle_fail_test "long castling with queen blocking" "O-O-O"
      [ long_castling_with_queen_in_path_board ]
      White (Failure "Squares occupied");
    castle_fail_test "short castling with knight blocking" "O-O"
      [ short_castling_with_knight_in_path_board ]
      White (Failure "Squares occupied");
    castle_fail_test "Can't Castle, King in Check" "O-O"
      [ short_castling_in_check_board ]
      White (Failure "Can't castle in check");
    castle_fail_test "Short Castle Rook first move false" "O-O"
      [ short_castling_rook_moved_board ]
      White (Failure "Rook must be on original square");
    castle_fail_test "Long Castle Rook first move false" "O-O-O"
      [ long_castling_rook_moved_board ]
      White (Failure "Rook must be on original square");
  ]

let capture_tests =
  [
    specific_piece_moves_test "really just a test for the test methods"
      [ "a3"; "a4" ]
      (unopt_pieces (get_piece_list White single_pawn_board))
      [ ((1, 2), (1, 3)); ((1, 2), (1, 4)) ]
      single_pawn_board;
    start_end_coord_test "Rook Ra5" "Rxa5" board_rook_capturing_pieces
      [ (6, 5); (1, 5) ]
      board_rook_capturing;
    start_end_coord_test "Rook Rf8" "Rxf8" board_rook_capturing_pieces
      [ (6, 5); (6, 8) ]
      board_rook_capturing;
    start_end_coord_test "Rook Rfg5" "Rfxg5" board_rook_capturing_pieces
      [ (6, 5); (7, 5) ]
      board_rook_capturing;
    start_end_coord_test "Rook Rxa5" "Rxa5" board_rook_capturing_pieces
      [ (6, 5); (1, 5) ]
      board_rook_capturing;
    start_end_coord_test "Rook Rxf8" "Rxf8" board_rook_capturing_pieces
      [ (6, 5); (6, 8) ]
      board_rook_capturing;
    start_end_coord_test "Rook Rfxg5" "Rfxg5" board_rook_capturing_pieces
      [ (6, 5); (7, 5) ]
      board_rook_capturing;
    start_end_coord_test "Rook Rb5" "Rxb5" board_rook_capturing_pieces
      [ (6, 5); (2, 5) ]
      board_rook_capturing;
    start_end_coord_test "Rook Rff7" "Rfxf7" board_rook_capturing_pieces
      [ (6, 5); (6, 7) ]
      board_rook_capturing;
    (*knight*)
    start_end_coord_test "Knight Nb6" "Nxb6" board_knight_capturing_pieces
      [ (4, 5); (2, 6) ]
      board_knight_capturing;
    start_end_coord_test "Knight Nc7" "Nxc7" board_knight_capturing_pieces
      [ (4, 5); (3, 7) ]
      board_knight_capturing;
    start_end_coord_test "Knight Ne7" "Nxe7" board_knight_capturing_pieces
      [ (4, 5); (5, 7) ]
      board_knight_capturing;
    start_end_coord_test "Knight Nxb6" "Nxb6" board_knight_capturing_pieces
      [ (4, 5); (2, 6) ]
      board_knight_capturing;
    start_end_coord_test "Knight Nxc7" "Nxc7" board_knight_capturing_pieces
      [ (4, 5); (3, 7) ]
      board_knight_capturing;
    start_end_coord_test "Knight Nxe7" "Nxe7" board_knight_capturing_pieces
      [ (4, 5); (5, 7) ]
      board_knight_capturing;
    (*bishop*)
    start_end_coord_test "Bishop Ba7" "Bxa7" board_bishop_capturing_pieces
      [ (4, 4); (1, 7) ]
      board_bishop_capturing;
    start_end_coord_test "Bishop Bb2" "Bxb2" board_bishop_capturing_pieces
      [ (4, 4); (2, 2) ]
      board_bishop_capturing;
    start_end_coord_test "Bishop Be3" "Bxe3" board_bishop_capturing_pieces
      [ (4, 4); (5, 3) ]
      board_bishop_capturing;
    start_end_coord_test "Bishop Bh8" "Bxh8" board_bishop_capturing_pieces
      [ (4, 4); (8, 8) ]
      board_bishop_capturing;
    start_end_coord_test "Bishop Bxa7" "Bxa7" board_bishop_capturing_pieces
      [ (4, 4); (1, 7) ]
      board_bishop_capturing;
    start_end_coord_test "Bishop Bxb2" "Bxb2" board_bishop_capturing_pieces
      [ (4, 4); (2, 2) ]
      board_bishop_capturing;
    start_end_coord_test "Bishop Bxe3" "Bxe3" board_bishop_capturing_pieces
      [ (4, 4); (5, 3) ]
      board_bishop_capturing;
    start_end_coord_test "Bishop Bxh8" "Bxh8" board_bishop_capturing_pieces
      [ (4, 4); (8, 8) ]
      board_bishop_capturing;
    start_end_coord_test "Bishop Bc3" "Bxc3" board_bishop_capturing_pieces
      [ (4, 4); (3, 3) ]
      board_bishop_capturing;
    start_end_coord_test "Bishop Bb6" "Bxb6" board_bishop_capturing_pieces
      [ (4, 4); (2, 6) ]
      board_bishop_capturing;
    start_end_coord_test "Bishop Bg7" "Bxg7" board_bishop_capturing_pieces
      [ (4, 4); (7, 7) ]
      board_bishop_capturing;
  ]

let start_end_coord_tests =
  [
    start_end_coord_test
      "User input Nc3 has start\n\
      \  and end coordinates (2,1) and (3,3) with board  initial"
      (*Knight jumps over pieces*) "Nc3" start_pieces
      [ (2, 1); (3, 3) ]
      init_board;
  ]

let exception_start_end_coord_tests =
  [
    exception_start_end_coord_test "Rook\n  can't jump over pawn in init board"
      "Ra4" start_pieces (Failure "no pieces can move here") init_board;
    exception_start_end_coord_test "Pawn can't move 3\n  spaces in init board"
      "a5" start_pieces (Failure "no pieces can move here") init_board;
    exception_start_end_coord_test
      "Only other player's piece can\n  move here in init board" "a6"
      start_pieces (Failure "no pieces can move here") init_board;
  ]

let undo_tests =
  [
    exception_test "Cannot undo with no board" [];
    exception_test "Cannot undo with empty board" [ init_board ];
    exception_test "Cannot undo with one move"
      (move_piece ((2, 4), (2, 5)) [ init_board ] |> snd);
    board_test "Successful undo" init_board (revert_state pawn_moves |> List.hd);
  ]

let enpassant_tests =
  [
    check_enp_test "Possible enpassant" (2, 5) (1, 6) enp_captured_move true;
    check_enp_test "Insufficient board setup" (2, 5) (1, 6) [] false;
    check_enp_test "Not enough previous move" (2, 5) (1, 6)
      (move_piece ((2, 4), (2, 5)) [ init_board ] |> snd)
      false;
    check_enp_test "Capturing piece not pawn" (5, 1) (1, 3) enp_captured_move
      false;
    check_enp_test "No capture occurred" (2, 5) (2, 6) enp_captured_move false;
    check_enp_test "Past enpassant capture" (2, 5) (1, 6) past_enp_capture false;
    check_enp_test "No capturing piece" (5, 5) (5, 6) past_enp_capture false;
    check_enp_test "No captured piece" (2, 5) (3, 6) past_enp_capture false;
    remove_enp_state_test "Empty board returns the same board" [] [];
    remove_enp_state_test "One board returns the same board" [ init_board ]
      [ init_board ];
    remove_enp_state_test "Removes one state"
      (move_piece ((2, 4), (2, 5)) [ init_board ] |> snd)
      [ init_board ];
  ]

let init_board_has_pieces (y_coord : int) : test list =
  List.map
    (fun x ->
      "row " ^ string_of_int y_coord ^ "has pieces" >:: fun _ ->
      assert_equal true (has_piece (x, y_coord) init_board))
    (gen_int 1 8)

let init_board_has_pieces_tests =
  init_board_has_pieces 1 @ init_board_has_pieces 2
  |> List.append (init_board_has_pieces 7)
  |> List.append (init_board_has_pieces 8)

let color_piece_test (name : string) (color : color) (board : Square.t Board.t)
    (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal expected_output (get_piece_list color board |> List.length)

let pawn_move_board = update_piece_loc (4, 2) (4, 4) init_board |> snd
let knight_move_board = update_piece_loc (2, 1) (3, 3) init_board |> snd
let queen_move_board = update_piece_loc (4, 1) (4, 3) pawn_move_board |> snd

let has_piece_test (name : string) (loc : point) (board : Square.t Board.t) :
    test =
  name >:: fun _ -> assert_equal true (has_piece loc board)

let promotion_tests =
  [
    promote_test "Pawn to Queen" [ promotion_board ] (1, 8) White "a8=Q" p;
    promote_test "Pawn to Knight" [ promotion_board ] (1, 8) White "a8=N"
      { p with piece_type = Knight knight };
    promote_test "Pawn to Rook" [ promotion_board ] (1, 8) White "a8=R"
      { p with piece_type = Rook rook };
    promote_test "Pawn to Bishop" [ promotion_board ] (1, 8) White "a8=B"
      { p with piece_type = Bishop bishop };
    promote_test "Promotion without =" [ promotion_board ] (1, 8) White "a8" p;
    promote_fail_test "Promotion with invalid promotion piece"
      [ promotion_board ] (1, 8) White "a8=p" (Failure "Invalid promotion");
  ]

let insufficient_material_tests =
  [
    insufficient_material_test "Lone kings"
      (add_pieces
         [
           {
             piece_type = King king;
             location = (5, 1);
             color = White;
             first_move = false;
           };
           {
             piece_type = King king;
             location = (6, 1);
             color = Black;
             first_move = false;
           };
         ]
         Board.empty)
      true;
    insufficient_material_test "Lone king vs king and a bishop"
      (add_pieces
         [
           {
             piece_type = King king;
             location = (5, 1);
             color = White;
             first_move = false;
           };
           {
             piece_type = King king;
             location = (6, 1);
             color = Black;
             first_move = false;
           };
           {
             piece_type = Bishop bishop;
             location = (7, 1);
             color = Black;
             first_move = false;
           };
         ]
         Board.empty)
      true;
    insufficient_material_test "Lone king vs king and a knight"
      (add_pieces
         [
           {
             piece_type = Knight knight;
             location = (7, 1);
             color = Black;
             first_move = false;
           };
           {
             piece_type = King king;
             location = (5, 1);
             color = White;
             first_move = false;
           };
           {
             piece_type = King king;
             location = (6, 1);
             color = Black;
             first_move = false;
           };
         ]
         Board.empty)
      true;
    insufficient_material_test "king and a bishop vs king and a bishop"
      (add_pieces
         [
           {
             piece_type = Bishop bishop;
             location = (7, 1);
             color = Black;
             first_move = false;
           };
           {
             piece_type = Bishop bishop;
             location = (1, 1);
             color = White;
             first_move = false;
           };
           {
             piece_type = King king;
             location = (5, 1);
             color = White;
             first_move = false;
           };
           {
             piece_type = King king;
             location = (6, 1);
             color = Black;
             first_move = false;
           };
         ]
         Board.empty)
      true;
    insufficient_material_test "king and a pawn vs king"
      (add_pieces
         [
           {
             piece_type = Pawn pawn;
             location = (7, 1);
             color = Black;
             first_move = false;
           };
           {
             piece_type = King king;
             location = (5, 1);
             color = White;
             first_move = false;
           };
           {
             piece_type = King king;
             location = (6, 1);
             color = Black;
             first_move = false;
           };
         ]
         Board.empty)
      false;
    insufficient_material_test "king and multiple pieces vs king"
      (add_pieces
         [
           {
             piece_type = Pawn pawn;
             location = (7, 1);
             color = White;
             first_move = false;
           };
           {
             piece_type = King king;
             location = (5, 1);
             color = White;
             first_move = false;
           };
           {
             piece_type = Bishop bishop;
             location = (1, 1);
             color = White;
             first_move = false;
           };
           {
             piece_type = King king;
             location = (6, 1);
             color = Black;
             first_move = false;
           };
         ]
         Board.empty)
      false;
  ]

let has_no_legal_move_tests =
  [
    has_no_legal_move_test "double check"
      (add_pieces
         [
           {
             piece_type = Rook rook;
             location = (1, 2);
             color = White;
             first_move = false;
           };
           {
             piece_type = Rook rook;
             location = (1, 1);
             color = White;
             first_move = false;
           };
           {
             piece_type = King king;
             location = (5, 1);
             color = White;
             first_move = false;
           };
           {
             piece_type = Bishop bishop;
             location = (1, 1);
             color = White;
             first_move = false;
           };
           {
             piece_type = King king;
             location = (6, 1);
             color = Black;
             first_move = false;
           };
         ]
         Board.empty)
      White false;
    has_no_legal_move_test "double check"
      (add_pieces
         [
           {
             piece_type = Rook rook;
             location = (1, 2);
             color = Black;
             first_move = false;
           };
           {
             piece_type = Rook rook;
             location = (1, 1);
             color = Black;
             first_move = false;
           };
           {
             piece_type = King king;
             location = (5, 1);
             color = White;
             first_move = false;
           };
         ]
         Board.empty)
      White true;
    has_no_legal_move_test "double check"
      (add_pieces
         [
           {
             piece_type = Rook rook;
             location = (1, 2);
             color = Black;
             first_move = false;
           };
           {
             piece_type = Rook rook;
             location = (1, 1);
             color = Black;
             first_move = false;
           };
           {
             piece_type = King king;
             location = (5, 1);
             color = White;
             first_move = false;
           };
         ]
         Board.empty)
      White true;
  ]

let illegal_jump_tests =
  [
    illegal_jump_test "White King" true
      (get_piece (5, 1) init_board)
      (4, 7) init_board false;
    illegal_jump_test "Black Pawn" false
      (get_piece (4, 7) init_board)
      (4, 6) init_board false;
    illegal_jump_test "Black Pawn" false
      (get_piece (4, 7) init_board)
      (4, 6) init_board false;
    illegal_jump_test "Has piece" false
      (get_piece (1, 1) illegal_jump_has_piece)
      (1, 2) illegal_jump_has_piece true;
    illegal_jump_test "Has no piece" false
      (get_piece (1, 2) illegal_jump_has_piece)
      (1, 3) illegal_jump_has_piece false;
  ]

let check_piece_loc (name : string) (loc : point) (board : Square.t Board.t) :
    test =
  name >:: fun _ -> assert_equal loc (get_piece loc board).location

let tests =
  "chess test suite"
  >::: init_board_has_pieces_tests
       @ [
           color_piece_test "init board check white pieces" White init_board 16;
           color_piece_test "init board check black pieces" Black init_board 16;
           has_piece_test "has pawn at (4, 4)" (4, 4) pawn_move_board;
           has_piece_test "has knight at (3, 3)" (3, 3) knight_move_board;
           has_piece_test "has pawn at\n  (4, 4) after queen moves" (4, 4)
             pawn_move_board;
           has_piece_test "has queen at\n  (4, 3)" (4, 3) queen_move_board;
           check_piece_loc "check pawn loc at (4, 4)" (4, 4) pawn_move_board;
           check_piece_loc "check knight loc at (3, 3)" (3, 3) knight_move_board;
           check_piece_loc "check queen loc at (4, 3)" (4, 3) queen_move_board;
           color_piece_test "check white pieces after some moves" White
             queen_move_board 16;
           color_piece_test "check black pieces after some moves" Black
             queen_move_board 16;
         ]
       @ List.flatten
           [
             all_white_pawns_tests;
             start_end_coord_tests;
             exception_start_end_coord_tests;
             undo_tests;
             enpassant_tests;
             castling_tests;
             promotion_tests;
             has_no_legal_move_tests;
             illegal_jump_tests;
             insufficient_material_tests;
             into_check_illegal_tests;
             castling_tests;
             check_opponent_tests;
             special_pawns_tests;
             capture_tests;
           ]

let _ = run_test_tt_main tests
