open Game
open State
open Command
open Piece
open Board
open Square
open Gui
open Helper

exception PlayModeSelectionError

let instruction () extend =
  print_string "\nUse ";
  ANSITerminal.print_string [ ANSITerminal.yellow ] "algebraic notation";
  print_string " to make moves. \n";
  if extend then (
    print_endline
      "     i. Chess notation uses abbreviations for each piece, using \
       capitalized letters.";
    print_endline
      "        King = K, Queen = Q, Bishop = B, Knight = N, Rook = R, Pawn has \
       no notation.";
    print_endline "        For example, Ba4 indicates moving Bishop to a4";
    print_endline "     ii. Use \"O-O-O\" or \"0-0-0 for castling short.";
    print_endline "     iii. Use \"O-O\" or \"0-0\" for castling long.";
    print_endline "     iv. Use \"x\" to capture pieces.";
    print_endline
      "         For example, Nxe4 indicates Knight to capture pieces on e4.");
  print_string "Enter ";
  ANSITerminal.print_string [ ANSITerminal.yellow ] "Resign";
  print_endline " to resign.";
  print_string "Enter ";
  ANSITerminal.print_string [ ANSITerminal.yellow ] "Draw";
  print_endline " to prompt a draw offer to your opponent.";
  if extend then (
    print_endline
      "     i. The player that prompt the draw offer has to make moves after \
       offering to draw.";
    print_endline
      "     ii. Opponent can decline the draw by playing a move, or agree to \
       the draw by entering \"Draw\".";
    print_endline "     iii. Draw offers are only valid for one turn.";
    print_endline "     iv. Players can make multiple draw offers.");
  print_string "Enter ";
  ANSITerminal.print_string [ ANSITerminal.yellow ] "Undo";
  print_endline " to revert to your previous move.";
  if extend then (
    print_endline "     i. You can only undo moves when it is your turn.";
    print_endline "     ii. You can undo multiple moves.");
  print_string "Enter ";
  ANSITerminal.print_string [ ANSITerminal.yellow ] "Help";
  print_endline " to get help."

(* Reference to store state of draw for each player*)
let white_draw = ref false
let blk_draw = ref false

let enpassant_move input state_lst turn =
  let start, endpoint = enpassant_points input turn in
  let st = get_first_state state_lst in
  if check_enpassant start endpoint state_lst then (
    let rem_state = remove_piece (endpoint |> fst, start |> snd) st in
    ignore rem_state;
    let new_state =
      move_piece (start, endpoint) (rem_state :: state_lst) |> snd
    in
    (true, new_state |> remove_enp_state))
  else (false, state_lst)

(* [playing_game st turn] returns [st] the state of the board with the color
   [turn] to move *)
let rec playing_game state_lst turn =
  let st = get_first_state state_lst in
  try
    if has_no_legal_move st turn then
      if is_in_check st turn then
        if turn = White then (
          print_endline "\n Black wins by checkmate";
          exit 0)
        else (
          print_endline "\n White wins by checkmate";
          exit 0)
      else (
        print_endline "\n Draw by stalemate";
        exit 0);
    if insufficient_material st then (
      print_endline "\n Draw by insufficient material";
      exit 0);
    if is_in_check st turn then print_endline "\n You are in check!";
    if !white_draw && !blk_draw then (
      print_endline " \n The game has ended in a mutual draw\n";
      exit 0);
    if turn = White then print_string "\n White to move: "
    else print_string "\n Black to move: ";
    let input = read_line () in
    ignore input;
    if input = "Help" then (
      instruction () true;
      playing_game state_lst turn);
    if input = "Resign" then (
      if turn = White then print_endline " Black wins\n"
      else print_endline " White wins\n";
      exit 0);
    if input = "Draw" then (
      if turn = White then white_draw := true else blk_draw := true;
      playing_game state_lst turn);
    if input = "Undo" then (
      let rev_state = revert_state state_lst in
      print_endline " Successfully undo move!";
      print_string (print_board (get_first_state rev_state));
      playing_game rev_state turn);
    (if input_enpassant input st turn then
     let res, newstate = enpassant_move input state_lst turn in
     if res then (
       print_string (print_board (get_first_state newstate));
       playing_game newstate (not_color turn)));
    if input = "O-O" || input = "O-O-O" || input = "0-0" || input = "0-0-0" then (
      let newstate = snd (castle input state_lst turn) in
      if is_in_check (get_first_state newstate) turn then
        failwith "You are still in check"
      else print_string (print_board (get_first_state newstate));
      playing_game newstate (not_color turn));
    let input2 =
      if List.length (String.split_on_char '=' input) > 1 then
        List.hd (String.split_on_char '=' input)
      else input
    in
    if turn = White && !blk_draw then blk_draw := false;
    if turn = Black && !white_draw then white_draw := false;
    let action =
      start_end_coord input2 (unopt_pieces (get_piece_list turn st)) st
    in
    ignore action;
    let coords = (List.hd action, List.nth action 1) in
    ignore coords;
    let newstate =
      if
        (snd (snd coords)
        =
        match turn with
        | White -> 8
        | Black -> 1)
        && (match get_piece (fst coords) st with
           | None -> failwith "No piece moved"
           | Some x -> x)
             .piece_type = Pawn pawn
      then promote (snd (move_piece coords state_lst)) (snd coords) turn input
      else snd (move_piece coords state_lst)
    in
    if is_in_check (List.hd newstate) turn then
      failwith "You are still in check"
    else print_string (print_board (get_first_state newstate));
    playing_game newstate (not_color turn)
  with
  | Empty ->
      print_endline " Please enter a nonempty move";
      playing_game state_lst turn
  | Malformed ->
      print_endline "Please enter a valid move";
      playing_game state_lst turn
  | UndoFailure ->
      print_endline " Unable to undo move. Please play a valid move.";
      playing_game state_lst turn
  | Failure _ | Invalid_argument _ ->
      print_endline "Illegal move, enter another move";
      playing_game state_lst turn


(*[play_mode_print] calls terminal to outprint possible playmode for players to select*)
let play_mode_print () =
  ANSITerminal.print_string [ ANSITerminal.yellow ] "Choose a play mode.\n";
  print_endline " 1. Classical Chess";
  print_endline " 2. Anarchy Chess \n"

(*[play_mode'] is a helper function to [play_mode]. It interprets the player's input*)
let play_mode' input =
  match String.lowercase_ascii input with
  | "1" -> get_init_board
  | "classical chess" -> get_init_board
  | "2" -> get_init_anar_board
  | "anarchy chess" -> get_init_anar_board
  | _ -> raise PlayModeSelectionError
  
(*[play_mode] is handles the playmode selection mechanism*)
let rec play_mode input =
  try play_mode' input
  with PlayModeSelectionError ->
    play_mode_print ();
    play_mode (read_line ())

(* [main ()] creates the chess game. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to the 3110 chess program.\n";
  play_mode_print ();
  let input = read_line () in
  ignore input;
  let st = play_mode input in
  ignore st;
  instruction () false;
  ANSITerminal.print_string [ ANSITerminal.red ] "Have fun!";
  print_string (print_board st);
  let output = playing_game [ st ] White in
  ignore output

(* Execute the chess game. *)
let () = main ()