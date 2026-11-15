open Tetris.Controls
open Tetris.Input_decoder

let test_regular_key_decoding () =
  assert
    (match Tetris.Input_decoder.consume Tetris.Input_decoder.Initial 'w' with
     | Command (Game_action Tetris.Types.Rotate_clockwise) -> true
     | _ -> false)

let test_arrow_sequence_decoding () =
  assert
    (match Tetris.Input_decoder.consume Tetris.Input_decoder.Initial '\027' with
     | Awaiting_input Escape -> true
     | _ -> false);
  assert
    (match Tetris.Input_decoder.consume Escape '[' with
     | Awaiting_input Arrow -> true
     | _ -> false);
  assert
    (match Tetris.Input_decoder.consume Arrow 'D' with
     | Command (Game_action Tetris.Types.Move_left) -> true
     | _ -> false)

let test_escape_timeout_quits () =
  assert
    (match Tetris.Input_decoder.timeout Tetris.Input_decoder.Escape with
     | Command Quit -> true
     | _ -> false)

let test_malformed_escape_sequences_are_ignored () =
  assert
    (match Tetris.Input_decoder.consume Tetris.Input_decoder.Escape 'x' with
     | Ignored -> true
     | _ -> false);
  assert
    (match Tetris.Input_decoder.consume Tetris.Input_decoder.Arrow 'x' with
     | Ignored -> true
     | _ -> false)

let () =
  test_regular_key_decoding ();
  test_arrow_sequence_decoding ();
  test_escape_timeout_quits ();
  test_malformed_escape_sequences_are_ignored ()
