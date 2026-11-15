open Base
open Tetris.Types

let same_piece left right =
  match left, right with
  | I, I
  | O, O
  | T, T
  | J, J
  | L, L
  | S, S
  | Z, Z -> true
  | _ -> false

let test_piece_cannot_move_past_left_wall () =
  let game = Tetris.Game.create ~first_piece:Tetris.Types.O () in
  let after_moves =
    List.fold (List.range 0 10) ~init:game ~f:(fun state _ ->
      Tetris.Game.apply_action state Tetris.Types.Move_left)
  in
  assert (after_moves.active_piece.origin.column = 0)

let test_tick_moves_the_active_piece_down () =
  let game = Tetris.Game.create ~first_piece:Tetris.Types.O () in
  let after_tick = Tetris.Game.apply_action game Tetris.Types.Tick in
  assert (after_tick.active_piece.origin.row = game.active_piece.origin.row + 1)

let test_hard_drop_locks_and_spawns_next_piece () =
  let game = Tetris.Game.create ~first_piece:Tetris.Types.O ~next_piece:Tetris.Types.I () in
  let game = Tetris.Game.apply_action game Tetris.Types.Hard_drop in
  assert (match game.active_piece.kind with Tetris.Types.I -> true | _ -> false);
  assert (not (Tetris.Board.is_empty game.board { Tetris.Types.column = 4; row = 19 }))

let test_blocked_spawn_ends_the_game () =
  let game = Tetris.Game.create ~first_piece:Tetris.Types.O ~next_piece:Tetris.Types.T () in
  let board = Tetris.Board.place game.board (Tetris.Piece.spawn Tetris.Types.T) in
  let game = { game with board } in
  let game = Tetris.Game.apply_action game Tetris.Types.Hard_drop in
  assert (match game.phase with Tetris.Types.Game_over -> true | Playing -> false)

let test_preview_advances_after_a_piece_locks () =
  let seed = 17 in
  let randomizer = Tetris.Randomizer.create ~seed in
  let _, randomizer = Tetris.Randomizer.next randomizer in
  let _, randomizer = Tetris.Randomizer.next randomizer in
  let expected_preview, _ = Tetris.Randomizer.next randomizer in
  let game = Tetris.Game.create ~seed ~first_piece:Tetris.Types.O ~next_piece:Tetris.Types.I () in
  let game = Tetris.Game.apply_action game Tetris.Types.Hard_drop in
  assert (same_piece game.next_piece expected_preview)

let test_tick_locks_a_piece_at_the_floor () =
  let game = Tetris.Game.create ~first_piece:Tetris.Types.O ~next_piece:Tetris.Types.I () in
  let active_piece = Tetris.Piece.move game.active_piece ~columns:0 ~rows:20 in
  let game = { game with active_piece } in
  let game = Tetris.Game.apply_action game Tetris.Types.Tick in
  assert (match game.active_piece.kind with Tetris.Types.I -> true | _ -> false);
  assert (not (Tetris.Board.is_empty game.board { Tetris.Types.column = 4; row = 19 }))

let points_for_lines = function
  | 1 -> 100
  | 2 -> 300
  | 3 -> 500
  | 4 -> 800
  | _ -> assert false

let test_scoring_for_line_clears () =
  List.iter [ 1; 2; 3; 4 ] ~f:(fun line_count ->
    let game = Tetris.Game.create ~first_piece:Tetris.Types.I ~next_piece:Tetris.Types.O () in
    let board = Tetris.Board.empty () in
    for row = 20 - line_count to 19 do
      for column = 0 to 8 do
        board.(Tetris.Board.storage_row row).(column) <- Tetris.Types.Filled
      done
    done;
    let active_piece =
      Tetris.Piece.spawn Tetris.Types.I
      |> Tetris.Piece.rotate_clockwise
      |> Tetris.Piece.move ~columns:4 ~rows:19
    in
    let game = { game with board; active_piece } in
    let game = Tetris.Game.apply_action game Tetris.Types.Tick in
    assert (game.lines_cleared = line_count);
    assert (game.score = points_for_lines line_count))

let () =
  test_piece_cannot_move_past_left_wall ();
  test_tick_moves_the_active_piece_down ();
  test_hard_drop_locks_and_spawns_next_piece ();
  test_blocked_spawn_ends_the_game ();
  test_preview_advances_after_a_piece_locks ();
  test_tick_locks_a_piece_at_the_floor ();
  test_scoring_for_line_clears ()
