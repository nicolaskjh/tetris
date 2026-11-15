open Base
open Types

let piece_kind_to_string = function
  | I -> "I"
  | O -> "O"
  | T -> "T"
  | J -> "J"
  | L -> "L"
  | S -> "S"
  | Z -> "Z"

let phase_to_string = function
  | Playing -> "Playing"
  | Game_over -> "Game over"

let is_active_square (game : Game.t) coordinate =
  List.exists (Piece.squares game.active_piece) ~f:(fun square ->
    square.column = coordinate.column && square.row = coordinate.row)

let character_at (game : Game.t) coordinate =
  if is_active_square game coordinate then '#'
  else
    match Board.cell game.board coordinate with
    | Empty -> ' '
    | Filled -> '#'

let render_row (game : Game.t) row =
  let contents =
    String.init Board.width ~f:(fun column -> character_at game { column; row })
  in
  if row < 0 then " " ^ contents ^ "\n" else "|" ^ contents ^ "|\n"

let border = "+" ^ String.make Board.width '-' ^ "+\n"

let render (game : Game.t) =
  let rows = List.map (List.range Board.first_row (Board.last_row + 1)) ~f:(render_row game) in
  String.concat ~sep:""
    (rows
     @ [ border
       ; "Score: " ^ Int.to_string game.score ^ "  Lines: " ^ Int.to_string game.lines_cleared ^ "\n"
       ; "Next: " ^ piece_kind_to_string game.next_piece ^ "  " ^ phase_to_string game.phase ^ "\n"
       ])
