open Base

let move_to piece ~column ~row =
  Tetris.Piece.move piece ~columns:(column - piece.origin.column) ~rows:(row - piece.origin.row)

let test_empty_board_accepts_spawn_piece () =
  let board = Tetris.Board.empty () in
  assert (Tetris.Board.can_place board (Tetris.Piece.spawn Tetris.Types.T))

let test_hidden_rows_accept_a_rotated_spawn_piece () =
  let board = Tetris.Board.empty () in
  let piece = Tetris.Piece.spawn Tetris.Types.I |> Tetris.Piece.rotate_clockwise in
  assert (Tetris.Board.can_place board piece)

let test_with_filled_cells_creates_a_fixture_board () =
  let board =
    Tetris.Board.with_filled_cells
      [ { Tetris.Types.column = 0; row = 0 }; { column = 9; row = 19 } ]
  in
  assert (not (Tetris.Board.is_empty board { Tetris.Types.column = 0; row = 0 }));
  assert (not (Tetris.Board.is_empty board { Tetris.Types.column = 9; row = 19 }))

let test_clear_two_completed_lines () =
  let board = Tetris.Board.empty () in
  let board =
    List.fold [ 0; 2; 4; 6; 8 ] ~init:board ~f:(fun board column ->
      let piece = Tetris.Piece.spawn Tetris.Types.O |> move_to ~column ~row:18 in
      Tetris.Board.place board piece)
  in
  let board, cleared_lines = Tetris.Board.clear_complete_lines board in
  assert (cleared_lines = 2);
  assert (Tetris.Board.is_empty board { Tetris.Types.column = 0; row = 19 })

let test_rows_above_a_clear_fall_down () =
  let board = Tetris.Board.empty () in
  let board = Tetris.Board.place board (Tetris.Piece.spawn Tetris.Types.O |> move_to ~column:0 ~row:17) in
  let board = Tetris.Board.place board (Tetris.Piece.spawn Tetris.Types.I |> move_to ~column:1 ~row:19) in
  let board = Tetris.Board.place board (Tetris.Piece.spawn Tetris.Types.I |> move_to ~column:5 ~row:19) in
  let board = Tetris.Board.place board (Tetris.Piece.spawn Tetris.Types.O |> move_to ~column:8 ~row:18) in
  let board, cleared_lines = Tetris.Board.clear_complete_lines board in
  assert (cleared_lines = 1);
  assert (Tetris.Board.is_empty board { Tetris.Types.column = 0; row = 17 });
  assert (not (Tetris.Board.is_empty board { Tetris.Types.column = 0; row = 18 }))

let test_clear_four_completed_lines () =
  let board = Tetris.Board.empty () in
  let board =
    List.fold [ 16; 18 ] ~init:board ~f:(fun board row ->
      List.fold [ 0; 2; 4; 6; 8 ] ~init:board ~f:(fun board column ->
        let piece = Tetris.Piece.spawn Tetris.Types.O |> move_to ~column ~row in
        Tetris.Board.place board piece))
  in
  let board, cleared_lines = Tetris.Board.clear_complete_lines board in
  assert (cleared_lines = 4);
  assert (Tetris.Board.is_empty board { Tetris.Types.column = 0; row = 19 })

let () =
  test_empty_board_accepts_spawn_piece ();
  test_hidden_rows_accept_a_rotated_spawn_piece ();
  test_with_filled_cells_creates_a_fixture_board ();
  test_clear_two_completed_lines ();
  test_rows_above_a_clear_fall_down ();
  test_clear_four_completed_lines ()
