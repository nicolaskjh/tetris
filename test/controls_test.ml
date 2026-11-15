open Base
open Tetris.Types
open Tetris.Controls

let test_known_keys_map_to_commands () =
  assert (match command_of_char 'a' with Some (Game_action Move_left) -> true | _ -> false);
  assert (match command_of_char 'd' with Some (Game_action Move_right) -> true | _ -> false);
  assert (match command_of_char 'w' with Some (Game_action Rotate_clockwise) -> true | _ -> false);
  assert (match command_of_char 's' with Some (Game_action Soft_drop) -> true | _ -> false);
  assert (match command_of_char ' ' with Some (Game_action Hard_drop) -> true | _ -> false);
  assert (match command_of_char 'q' with Some Quit -> true | _ -> false)

let test_arrow_keys_map_to_commands () =
  assert (match command_of_arrow 'A' with Some (Game_action Rotate_clockwise) -> true | _ -> false);
  assert (match command_of_arrow 'B' with Some (Game_action Soft_drop) -> true | _ -> false);
  assert (match command_of_arrow 'C' with Some (Game_action Move_right) -> true | _ -> false);
  assert (match command_of_arrow 'D' with Some (Game_action Move_left) -> true | _ -> false)

let test_unknown_keys_are_ignored () = assert (Option.is_none (command_of_char 'x'))

let () =
  test_known_keys_map_to_commands ();
  test_arrow_keys_map_to_commands ();
  test_unknown_keys_are_ignored ()
