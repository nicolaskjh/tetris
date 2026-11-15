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

let test_hold_stores_the_active_piece_and_spawns_the_preview () =
  let game = Tetris.Game.create ~first_piece:Tetris.Types.O ~next_piece:Tetris.Types.I () in
  let game = Tetris.Game.apply_action game Tetris.Types.Hold in
  assert (match game.held_piece with Some O -> true | _ -> false);
  assert (match game.active_piece.kind with I -> true | _ -> false);
  assert (not game.can_hold)

let test_hold_can_only_be_used_once_before_locking () =
  let game = Tetris.Game.create ~first_piece:Tetris.Types.O ~next_piece:Tetris.Types.I () in
  let game = Tetris.Game.apply_action game Tetris.Types.Hold in
  let game = Tetris.Game.apply_action game Tetris.Types.Hold in
  assert (match game.held_piece with Some O -> true | _ -> false);
  assert (match game.active_piece.kind with I -> true | _ -> false)

let test_locking_reenables_hold_and_a_second_hold_swaps_pieces () =
  let game = Tetris.Game.create ~first_piece:Tetris.Types.O ~next_piece:Tetris.Types.I () in
  let game = Tetris.Game.apply_action game Tetris.Types.Hold in
  let game = Tetris.Game.apply_action game Tetris.Types.Hard_drop in
  assert game.can_hold;
  let game = Tetris.Game.apply_action game Tetris.Types.Hold in
  assert (match game.active_piece.kind with O -> true | _ -> false);
  assert (not game.can_hold)

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

let game_with_piece_at_the_floor () =
  let game = Tetris.Game.create ~first_piece:Tetris.Types.O ~next_piece:Tetris.Types.I () in
  let active_piece = Tetris.Piece.move game.active_piece ~columns:0 ~rows:20 in
  { game with active_piece }

let test_tick_starts_lock_delay_at_the_floor () =
  let game = game_with_piece_at_the_floor () in
  let game = Tetris.Game.apply_action game Tetris.Types.Tick in
  assert (match game.active_piece.kind with Tetris.Types.O -> true | _ -> false);
  assert (game.grounded_ticks = 1);
  assert (Tetris.Board.is_empty game.board { Tetris.Types.column = 4; row = 19 })

let test_second_tick_locks_a_piece_at_the_floor () =
  let game = game_with_piece_at_the_floor () in
  let game = Tetris.Game.apply_action game Tetris.Types.Tick in
  let game = Tetris.Game.apply_action game Tetris.Types.Tick in
  assert (match game.active_piece.kind with Tetris.Types.I -> true | _ -> false);
  assert (not (Tetris.Board.is_empty game.board { Tetris.Types.column = 4; row = 19 }))

let test_successful_move_resets_lock_delay () =
  let game = game_with_piece_at_the_floor () in
  let game = Tetris.Game.apply_action game Tetris.Types.Tick in
  let game = Tetris.Game.apply_action game Tetris.Types.Move_left in
  assert (game.grounded_ticks = 0);
  let game = Tetris.Game.apply_action game Tetris.Types.Tick in
  assert (match game.active_piece.kind with Tetris.Types.O -> true | _ -> false);
  assert (game.grounded_ticks = 1)

let test_blocked_soft_drop_does_not_advance_lock_delay () =
  let game = game_with_piece_at_the_floor () in
  let game = Tetris.Game.apply_action game Tetris.Types.Soft_drop in
  assert (match game.active_piece.kind with Tetris.Types.O -> true | _ -> false);
  assert (game.grounded_ticks = 0)

let test_airborne_moves_do_not_consume_lock_resets () =
  let game = Tetris.Game.create ~first_piece:O ~next_piece:I () in
  let game = Tetris.Game.apply_action game Move_left in
  assert (game.lock_resets = 0)

let test_lock_delay_resets_are_capped () =
  let game = game_with_piece_at_the_floor () in
  let game = Tetris.Game.apply_action game Tick in
  let game =
    List.fold (List.range 0 Tetris.Game.max_lock_resets) ~init:game ~f:(fun game index ->
      let action = if index % 2 = 0 then Move_left else Move_right in
      Tetris.Game.apply_action game action)
  in
  assert (game.lock_resets = Tetris.Game.max_lock_resets);
  assert (game.grounded_ticks = 0);
  let game = Tetris.Game.apply_action game Tick in
  assert (game.grounded_ticks = 1);
  let game = Tetris.Game.apply_action game Move_left in
  assert (game.grounded_ticks = 1);
  let game = Tetris.Game.apply_action game Tick in
  assert (match game.active_piece.kind with I -> true | _ -> false)

let test_i_piece_kicks_away_from_the_right_wall () =
  let game = Tetris.Game.create ~first_piece:I ~next_piece:O () in
  let game =
    List.fold (List.range 0 3) ~init:game ~f:(fun game _ ->
      Tetris.Game.apply_action game Move_right)
  in
  let game = Tetris.Game.apply_action game Rotate_clockwise in
  let game = Tetris.Game.apply_action game Move_right in
  let game = Tetris.Game.apply_action game Rotate_clockwise in
  assert (match game.active_piece.rotation with Reverse -> true | _ -> false);
  assert (game.active_piece.origin.column = 7)

let test_t_piece_kicks_up_from_the_floor () =
  let game = Tetris.Game.create ~first_piece:T ~next_piece:O () in
  let game = Tetris.Game.apply_action game Rotate_clockwise in
  let game = Tetris.Game.apply_action game Rotate_clockwise in
  let game =
    List.fold (List.range 0 21) ~init:game ~f:(fun game _ ->
      Tetris.Game.apply_action game Soft_drop)
  in
  let game = Tetris.Game.apply_action game Rotate_clockwise in
  assert (match game.active_piece.rotation with Left -> true | _ -> false);
  assert (game.active_piece.origin.column = 5);
  assert (game.active_piece.origin.row = 18)

let test_rotation_fails_when_every_kick_is_blocked () =
  let game = Tetris.Game.create ~first_piece:T ~next_piece:O () in
  let active_piece =
    { (Tetris.Piece.spawn T) with
      rotation = Reverse
    ; origin = { column = 4; row = 19 }
    }
  in
  let board = game.board in
  board.(Tetris.Board.storage_row 18).(6) <- Filled;
  let game = { game with board; active_piece } in
  let game = Tetris.Game.apply_action game Rotate_clockwise in
  assert (match game.active_piece.rotation with Reverse -> true | _ -> false);
  assert (game.active_piece.origin.column = 4);
  assert (game.active_piece.origin.row = 19)

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
    let game = Tetris.Game.apply_action game Tetris.Types.Tick in
    assert (game.lines_cleared = line_count);
    assert (game.score = points_for_lines line_count))

let () =
  test_piece_cannot_move_past_left_wall ();
  test_tick_moves_the_active_piece_down ();
  test_hard_drop_locks_and_spawns_next_piece ();
  test_hold_stores_the_active_piece_and_spawns_the_preview ();
  test_hold_can_only_be_used_once_before_locking ();
  test_locking_reenables_hold_and_a_second_hold_swaps_pieces ();
  test_blocked_spawn_ends_the_game ();
  test_preview_advances_after_a_piece_locks ();
  test_tick_starts_lock_delay_at_the_floor ();
  test_second_tick_locks_a_piece_at_the_floor ();
  test_successful_move_resets_lock_delay ();
  test_blocked_soft_drop_does_not_advance_lock_delay ();
  test_airborne_moves_do_not_consume_lock_resets ();
  test_lock_delay_resets_are_capped ();
  test_i_piece_kicks_away_from_the_right_wall ();
  test_t_piece_kicks_up_from_the_floor ();
  test_rotation_fails_when_every_kick_is_blocked ();
  test_scoring_for_line_clears ()
