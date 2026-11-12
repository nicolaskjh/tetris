open Base

let coordinate_equal (left : Tetris.Types.coordinate) (right : Tetris.Types.coordinate) =
  left.column = right.column && left.row = right.row

let test_empty_board_accepts_spawn_piece () =
  let board = Tetris.Board.empty () in
  assert (Tetris.Board.can_place board (Tetris.Piece.spawn Tetris.Types.T))

let test_piece_cannot_move_past_left_wall () =
  let game = Tetris.Game.create ~first_piece:Tetris.Types.O () in
  let after_moves =
    List.fold (List.range 0 10) ~init:game ~f:(fun state _ ->
      Tetris.Game.apply_action state Tetris.Types.Move_left)
  in
  assert (after_moves.active_piece.origin.column = 0)

let test_o_piece_does_not_shift_when_rotated () =
  let piece = Tetris.Piece.spawn Tetris.Types.O in
  let rotated = Tetris.Piece.rotate_clockwise piece in
  assert (List.equal coordinate_equal (Tetris.Piece.squares piece) (Tetris.Piece.squares rotated))

let test_i_piece_uses_its_own_rotation_offsets () =
  let piece = Tetris.Piece.spawn Tetris.Types.I |> Tetris.Piece.rotate_clockwise in
  assert
    (List.equal coordinate_equal
       (Tetris.Piece.squares piece)
       [ { Tetris.Types.column = 5; row = -1 }
       ; { column = 5; row = 0 }
       ; { column = 5; row = 1 }
       ; { column = 5; row = 2 }
       ])

let test_hidden_rows_accept_a_rotated_spawn_piece () =
  let board = Tetris.Board.empty () in
  let piece = Tetris.Piece.spawn Tetris.Types.I |> Tetris.Piece.rotate_clockwise in
  assert (Tetris.Board.can_place board piece)

let () =
  test_empty_board_accepts_spawn_piece ();
  test_piece_cannot_move_past_left_wall ();
  test_o_piece_does_not_shift_when_rotated ();
  test_i_piece_uses_its_own_rotation_offsets ();
  test_hidden_rows_accept_a_rotated_spawn_piece ()
