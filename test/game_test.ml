open Base

let test_piece_cannot_move_past_left_wall () =
  let game = Tetris.Game.create ~first_piece:Tetris.Types.O () in
  let after_moves =
    List.fold (List.range 0 10) ~init:game ~f:(fun state _ ->
      Tetris.Game.apply_action state Tetris.Types.Move_left)
  in
  assert (after_moves.active_piece.origin.column = 0)

let test_hard_drop_locks_and_spawns_next_piece () =
  let game = Tetris.Game.create ~first_piece:Tetris.Types.O ~next_piece:Tetris.Types.I () in
  let game = Tetris.Game.apply_action game Tetris.Types.Hard_drop in
  assert (match game.active_piece.kind with Tetris.Types.I -> true | _ -> false);
  assert (not (Tetris.Board.is_empty game.board { Tetris.Types.column = 4; row = 19 }))

let () =
  test_piece_cannot_move_past_left_wall ();
  test_hard_drop_locks_and_spawns_next_piece ()
