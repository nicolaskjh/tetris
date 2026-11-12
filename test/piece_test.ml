open Base

let coordinate_equal (left : Tetris.Types.coordinate) (right : Tetris.Types.coordinate) =
  left.column = right.column && left.row = right.row

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

let () =
  test_o_piece_does_not_shift_when_rotated ();
  test_i_piece_uses_its_own_rotation_offsets ()
