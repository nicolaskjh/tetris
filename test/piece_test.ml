open Base

let coordinate_equal (left : Tetris.Types.coordinate) (right : Tetris.Types.coordinate) =
  left.column = right.column && left.row = right.row

let test_o_piece_does_not_shift_when_rotated () =
  let piece = Tetris.Piece.spawn Tetris.Types.O in
  let rotated = Tetris.Piece.rotate piece Tetris.Piece.Clockwise in
  assert (List.equal coordinate_equal (Tetris.Piece.squares piece) (Tetris.Piece.squares rotated))

let test_i_piece_uses_its_own_rotation_offsets () =
  let piece = Tetris.Piece.rotate (Tetris.Piece.spawn Tetris.Types.I) Tetris.Piece.Clockwise in
  assert
    (List.equal coordinate_equal
       (Tetris.Piece.squares piece)
       [ { Tetris.Types.column = 5; row = -3 }
       ; { column = 5; row = -2 }
       ; { column = 5; row = -1 }
       ; { column = 5; row = 0 }
       ])

let test_clockwise_and_counterclockwise_rotation_are_inverses () =
  List.iter
    [ Tetris.Types.I; Tetris.Types.O; Tetris.Types.T; Tetris.Types.J; Tetris.Types.L; Tetris.Types.S; Tetris.Types.Z ]
    ~f:(fun kind ->
    let piece = Tetris.Piece.spawn kind in
    let rotated = Tetris.Piece.rotate piece Tetris.Piece.Clockwise in
    let restored = Tetris.Piece.rotate rotated Tetris.Piece.Counterclockwise in
    assert (List.equal coordinate_equal (Tetris.Piece.squares piece) (Tetris.Piece.squares restored)))

let test_every_piece_spawns_above_the_visible_board () =
  List.iter [ Tetris.Types.I; O; T; J; L; S; Z ] ~f:(fun kind ->
    let piece = Tetris.Piece.spawn kind in
    List.iter (Tetris.Piece.squares piece) ~f:(fun square -> assert (square.row < 0)))

let () =
  test_o_piece_does_not_shift_when_rotated ();
  test_i_piece_uses_its_own_rotation_offsets ();
  test_clockwise_and_counterclockwise_rotation_are_inverses ();
  test_every_piece_spawns_above_the_visible_board ()
