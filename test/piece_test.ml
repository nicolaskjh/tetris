open Base
open Tetris.Types
open Tetris.Piece

let coordinate_equal (left : Tetris.Types.coordinate) (right : Tetris.Types.coordinate) =
  left.column = right.column && left.row = right.row

let rotation_equal left right =
  match left, right with
  | Spawn, Spawn | Right, Right | Reverse, Reverse | Left, Left -> true
  | _ -> false

let kick columns rows : Tetris.Piece.kick = { columns; rows }

let kick_equal (left : Tetris.Piece.kick) (right : Tetris.Piece.kick) =
  left.columns = right.columns && left.rows = right.rows

let all_rotations = [ Spawn; Right; Reverse; Left ]

let piece_with_rotation kind rotation : Tetris.Piece.t =
  { kind; rotation; origin = { column = 4; row = 5 } }

let expected_rotation direction rotation =
  match direction, rotation with
  | Tetris.Piece.Clockwise, Spawn -> Right
  | Clockwise, Right -> Reverse
  | Clockwise, Reverse -> Left
  | Clockwise, Left -> Spawn
  | Counterclockwise, Spawn -> Left
  | Counterclockwise, Right -> Spawn
  | Counterclockwise, Reverse -> Right
  | Counterclockwise, Left -> Reverse

let assert_kicks kind direction rotation expected =
  let piece = piece_with_rotation kind rotation in
  let actual = Tetris.Piece.kicks piece direction in
  assert (List.equal kick_equal actual expected)

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

let test_rotation_directions_change_each_orientation_correctly () =
  List.iter [ Tetris.Piece.Clockwise; Counterclockwise ] ~f:(fun direction ->
    List.iter all_rotations ~f:(fun rotation ->
      let piece = piece_with_rotation T rotation in
      let rotated = Tetris.Piece.rotate piece direction in
      assert (rotation_equal rotated.rotation (expected_rotation direction rotation))))

let test_srs_kick_tables () =
  let standard_spawn = [ kick 0 0; kick (-1) 0; kick (-1) (-1); kick 0 2; kick (-1) 2 ] in
  let standard_right = [ kick 0 0; kick 1 0; kick 1 1; kick 0 (-2); kick 1 (-2) ] in
  let standard_reverse = [ kick 0 0; kick 1 0; kick 1 (-1); kick 0 2; kick 1 2 ] in
  let standard_left = [ kick 0 0; kick (-1) 0; kick (-1) 1; kick 0 (-2); kick (-1) (-2) ] in
  let i_spawn = [ kick 0 0; kick (-2) 0; kick 1 0; kick (-2) 1; kick 1 (-2) ] in
  let i_right = [ kick 0 0; kick (-1) 0; kick 2 0; kick (-1) (-2); kick 2 1 ] in
  let i_reverse = [ kick 0 0; kick 2 0; kick (-1) 0; kick 2 (-1); kick (-1) 2 ] in
  let i_left = [ kick 0 0; kick 1 0; kick (-2) 0; kick 1 2; kick (-2) (-1) ] in
  let assert_kick_table kind direction expected_patterns =
    List.iter2_exn all_rotations expected_patterns ~f:(fun rotation expected ->
      assert_kicks kind direction rotation expected)
  in
  List.iter [ J; L; S; T; Z ] ~f:(fun kind ->
    assert_kick_table kind Clockwise [ standard_spawn; standard_right; standard_reverse; standard_left ];
    assert_kick_table kind Counterclockwise [ standard_reverse; standard_right; standard_spawn; standard_left ]);
  assert_kick_table I Clockwise [ i_spawn; i_right; i_reverse; i_left ];
  assert_kick_table I Counterclockwise [ i_right; i_reverse; i_left; i_spawn ];
  List.iter all_rotations ~f:(fun rotation ->
    List.iter [ Clockwise; Counterclockwise ] ~f:(fun direction ->
      assert_kicks O direction rotation [ kick 0 0 ]))

let test_every_piece_spawns_above_the_visible_board () =
  List.iter [ Tetris.Types.I; O; T; J; L; S; Z ] ~f:(fun kind ->
    let piece = Tetris.Piece.spawn kind in
    List.iter (Tetris.Piece.squares piece) ~f:(fun square -> assert (square.row < 0)))

let () =
  test_o_piece_does_not_shift_when_rotated ();
  test_i_piece_uses_its_own_rotation_offsets ();
  test_clockwise_and_counterclockwise_rotation_are_inverses ();
  test_rotation_directions_change_each_orientation_correctly ();
  test_srs_kick_tables ();
  test_every_piece_spawns_above_the_visible_board ()
