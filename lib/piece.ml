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

let rotate_clockwise piece =
  let rotation =
    match piece.rotation with
    | Spawn -> Right
    | Right -> Reverse
    | Reverse -> Left
    | Left -> Spawn
  in
  { piece with rotation }

let clockwise_kicks piece =
  let kick columns rows = { columns; rows } in
  match piece.kind, piece.rotation with
  | O, _ -> [ kick 0 0 ]
  | (J | L | S | T | Z), Spawn ->
      [ kick 0 0
      ; kick (-1) 0
      ; kick (-1) (-1)
      ; kick 0 2
      ; kick (-1) 2
      ]
  | (J | L | S | T | Z), Right ->
      [ kick 0 0
      ; kick 1 0
      ; kick 1 1
      ; kick 0 (-2)
      ; kick 1 (-2)
      ]
  | (J | L | S | T | Z), Reverse ->
      [ kick 0 0
      ; kick 1 0
      ; kick 1 (-1)
      ; kick 0 2
      ; kick 1 2
      ]
  | (J | L | S | T | Z), Left ->
      [ kick 0 0
      ; kick (-1) 0
      ; kick (-1) 1
      ; kick 0 (-2)
      ; kick (-1) (-2)
      ]
  | I, Spawn ->
      [ kick 0 0
      ; kick (-2) 0
      ; kick 1 0
      ; kick (-2) 1
      ; kick 1 (-2)
      ]
  | I, Right ->
      [ kick 0 0
      ; kick (-1) 0
      ; kick 2 0
      ; kick (-1) (-2)
      ; kick 2 1
      ]
  | I, Reverse ->
      [ kick 0 0
      ; kick 2 0
      ; kick (-1) 0
      ; kick 2 (-1)
      ; kick (-1) 2
      ]
  | I, Left ->
      [ kick 0 0
      ; kick 1 0
      ; kick (-2) 0
      ; kick 1 2
      ; kick (-2) (-1)
      ]
