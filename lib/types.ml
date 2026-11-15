open Base

type piece_kind =
  | I
  | O
  | T
  | J
  | L
  | S
  | Z

type cell =
  | Empty
  | Filled

type coordinate =
  { column : int
  ; row : int
  }

type rotation =
  | Spawn
  | Right
  | Reverse
  | Left

type phase =
  | Playing
  | Game_over

type action =
  | Move_left
  | Move_right
  | Rotate_clockwise
  | Rotate_counterclockwise
  | Soft_drop
  | Hard_drop
  | Hold
  | Tick
