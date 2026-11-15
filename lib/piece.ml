open Base
open Types

type t =
  { kind : piece_kind
  ; rotation : rotation
  ; origin : coordinate
  }

type kick =
  { columns : int
  ; rows : int
  }

type rotation_direction =
  | Clockwise
  | Counterclockwise

let spawn kind = { kind; rotation = Spawn; origin = { column = 4; row = -2 } }

let offsets kind rotation =
  match kind, rotation with
  | I, Spawn -> [ { column = -1; row = 0 }; { column = 0; row = 0 }; { column = 1; row = 0 }; { column = 2; row = 0 } ]
  | I, Right -> [ { column = 1; row = -1 }; { column = 1; row = 0 }; { column = 1; row = 1 }; { column = 1; row = 2 } ]
  | I, Reverse -> [ { column = -1; row = 1 }; { column = 0; row = 1 }; { column = 1; row = 1 }; { column = 2; row = 1 } ]
  | I, Left -> [ { column = 0; row = -1 }; { column = 0; row = 0 }; { column = 0; row = 1 }; { column = 0; row = 2 } ]
  | O, _ -> [ { column = 0; row = 0 }; { column = 1; row = 0 }; { column = 0; row = 1 }; { column = 1; row = 1 } ]
  | T, Spawn -> [ { column = -1; row = 0 }; { column = 0; row = 0 }; { column = 1; row = 0 }; { column = 0; row = 1 } ]
  | T, Right -> [ { column = 0; row = -1 }; { column = 0; row = 0 }; { column = 0; row = 1 }; { column = -1; row = 0 } ]
  | T, Reverse -> [ { column = 1; row = 0 }; { column = 0; row = 0 }; { column = -1; row = 0 }; { column = 0; row = -1 } ]
  | T, Left -> [ { column = 0; row = 1 }; { column = 0; row = 0 }; { column = 0; row = -1 }; { column = 1; row = 0 } ]
  | J, Spawn -> [ { column = -1; row = 0 }; { column = 0; row = 0 }; { column = 1; row = 0 }; { column = -1; row = 1 } ]
  | J, Right -> [ { column = 0; row = -1 }; { column = 0; row = 0 }; { column = 0; row = 1 }; { column = -1; row = -1 } ]
  | J, Reverse -> [ { column = 1; row = 0 }; { column = 0; row = 0 }; { column = -1; row = 0 }; { column = 1; row = -1 } ]
  | J, Left -> [ { column = 0; row = 1 }; { column = 0; row = 0 }; { column = 0; row = -1 }; { column = 1; row = 1 } ]
  | L, Spawn -> [ { column = -1; row = 0 }; { column = 0; row = 0 }; { column = 1; row = 0 }; { column = 1; row = 1 } ]
  | L, Right -> [ { column = 0; row = -1 }; { column = 0; row = 0 }; { column = 0; row = 1 }; { column = -1; row = 1 } ]
  | L, Reverse -> [ { column = 1; row = 0 }; { column = 0; row = 0 }; { column = -1; row = 0 }; { column = -1; row = -1 } ]
  | L, Left -> [ { column = 0; row = 1 }; { column = 0; row = 0 }; { column = 0; row = -1 }; { column = 1; row = -1 } ]
  | S, Spawn -> [ { column = 0; row = 0 }; { column = 1; row = 0 }; { column = -1; row = 1 }; { column = 0; row = 1 } ]
  | S, Right -> [ { column = 0; row = 0 }; { column = 0; row = 1 }; { column = -1; row = -1 }; { column = -1; row = 0 } ]
  | S, Reverse -> [ { column = 0; row = 0 }; { column = -1; row = 0 }; { column = 1; row = -1 }; { column = 0; row = -1 } ]
  | S, Left -> [ { column = 0; row = 0 }; { column = 0; row = -1 }; { column = 1; row = 1 }; { column = 1; row = 0 } ]
  | Z, Spawn -> [ { column = -1; row = 0 }; { column = 0; row = 0 }; { column = 0; row = 1 }; { column = 1; row = 1 } ]
  | Z, Right -> [ { column = 0; row = -1 }; { column = 0; row = 0 }; { column = -1; row = 0 }; { column = -1; row = 1 } ]
  | Z, Reverse -> [ { column = 1; row = 0 }; { column = 0; row = 0 }; { column = 0; row = -1 }; { column = -1; row = -1 } ]
  | Z, Left -> [ { column = 0; row = 1 }; { column = 0; row = 0 }; { column = 1; row = 0 }; { column = 1; row = -1 } ]

let squares piece =
  List.map (offsets piece.kind piece.rotation) ~f:(fun offset ->
    { column = piece.origin.column + offset.column; row = piece.origin.row + offset.row })

let move piece ~columns ~rows =
  { piece with origin = { column = piece.origin.column + columns; row = piece.origin.row + rows } }

let rotate piece direction =
  let rotation =
    match direction, piece.rotation with
    | Clockwise, Spawn -> Right
    | Clockwise, Right -> Reverse
    | Clockwise, Reverse -> Left
    | Clockwise, Left -> Spawn
    | Counterclockwise, Spawn -> Left
    | Counterclockwise, Right -> Spawn
    | Counterclockwise, Reverse -> Right
    | Counterclockwise, Left -> Reverse
  in
  { piece with rotation }

let kick columns rows = { columns; rows }

let standard_spawn_kicks = [ kick 0 0; kick (-1) 0; kick (-1) (-1); kick 0 2; kick (-1) 2 ]
let standard_right_kicks = [ kick 0 0; kick 1 0; kick 1 1; kick 0 (-2); kick 1 (-2) ]
let standard_reverse_kicks = [ kick 0 0; kick 1 0; kick 1 (-1); kick 0 2; kick 1 2 ]
let standard_left_kicks = [ kick 0 0; kick (-1) 0; kick (-1) 1; kick 0 (-2); kick (-1) (-2) ]

let i_spawn_kicks = [ kick 0 0; kick (-2) 0; kick 1 0; kick (-2) 1; kick 1 (-2) ]
let i_right_kicks = [ kick 0 0; kick (-1) 0; kick 2 0; kick (-1) (-2); kick 2 1 ]
let i_reverse_kicks = [ kick 0 0; kick 2 0; kick (-1) 0; kick 2 (-1); kick (-1) 2 ]
let i_left_kicks = [ kick 0 0; kick 1 0; kick (-2) 0; kick 1 2; kick (-2) (-1) ]

let standard_kicks direction rotation =
  match direction, rotation with
  | Clockwise, Spawn | Counterclockwise, Reverse -> standard_spawn_kicks
  | Clockwise, Right | Counterclockwise, Right -> standard_right_kicks
  | Clockwise, Reverse | Counterclockwise, Spawn -> standard_reverse_kicks
  | Clockwise, Left | Counterclockwise, Left -> standard_left_kicks

let i_kicks direction rotation =
  match direction, rotation with
  | Clockwise, Spawn | Counterclockwise, Left -> i_spawn_kicks
  | Clockwise, Right | Counterclockwise, Spawn -> i_right_kicks
  | Clockwise, Reverse | Counterclockwise, Right -> i_reverse_kicks
  | Clockwise, Left | Counterclockwise, Reverse -> i_left_kicks

let kicks piece direction =
  match piece.kind with
  | O -> [ kick 0 0 ]
  | I -> i_kicks direction piece.rotation
  | J | L | S | T | Z -> standard_kicks direction piece.rotation
