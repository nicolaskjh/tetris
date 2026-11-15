open Base
open Types

type t =
  { kind : piece_kind
  ; rotation : rotation
  ; origin : coordinate
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
