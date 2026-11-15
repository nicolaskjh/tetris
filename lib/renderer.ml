open Base
open Types

let cell_width = 2
let filled_cell = "██"
let ghost_cell = "░░"
let empty_cell = "  "

let phase_to_string = function
  | Playing -> "Playing"
  | Game_over -> "Game over"

let is_piece_square piece coordinate =
  List.exists (Piece.squares piece) ~f:(fun square ->
    square.column = coordinate.column && square.row = coordinate.row)

let rec ghost_piece (game : Game.t) piece =
  let below = Piece.move piece ~columns:0 ~rows:1 in
  if Board.can_place game.board below then ghost_piece game below else piece

let rendered_cell_at (game : Game.t) coordinate =
  if is_piece_square game.active_piece coordinate then filled_cell
  else if is_piece_square (ghost_piece game game.active_piece) coordinate then ghost_cell
  else
    match Board.cell game.board coordinate with
    | Empty -> empty_cell
    | Filled -> filled_cell

let render_row (game : Game.t) row =
  let contents =
    String.concat ~sep:""
      (List.init Board.width ~f:(fun column ->
         rendered_cell_at game { column; row }))
  in
  if row < 0 then " " ^ contents ^ " " else "|" ^ contents ^ "|"

let border = "+" ^ String.make (Board.width * cell_width) '-' ^ "+"

let min_offset_column offsets =
  match offsets with
  | [] -> assert false
  | first :: rest ->
      List.fold rest ~init:first.column ~f:(fun minimum offset ->
        if offset.column < minimum then offset.column else minimum)

let min_offset_row offsets =
  match offsets with
  | [] -> assert false
  | first :: rest ->
      List.fold rest ~init:first.row ~f:(fun minimum offset ->
        if offset.row < minimum then offset.row else minimum)

let render_next_piece kind =
  let offsets = Piece.offsets kind Spawn in
  let first_column = min_offset_column offsets in
  let first_row = min_offset_row offsets in
  List.init 2 ~f:(fun row ->
    String.concat ~sep:""
      (List.init 4 ~f:(fun column ->
         let column = first_column + column in
         let row = first_row + row in
         if List.exists offsets ~f:(fun offset -> offset.column = column && offset.row = row) then filled_cell
         else empty_cell)))

let sidebar_lines (game : Game.t) =
  let next_piece = render_next_piece game.next_piece in
  [ List.nth_exn next_piece 0
  ; List.nth_exn next_piece 1
  ; ""
  ; "Score: " ^ Int.to_string game.score
  ; "Lines: " ^ Int.to_string game.lines_cleared
  ]
  @
  match game.phase with
  | Playing -> []
  | Game_over -> [ ""; "+-----------+"; "| GAME OVER |"; "+-----------+" ]

let combine_lines board_lines sidebar_lines =
  let padding = Int.max 0 (List.length board_lines - List.length sidebar_lines) in
  let sidebar_lines = sidebar_lines @ List.init padding ~f:(fun _ -> "") in
  List.map2_exn board_lines sidebar_lines ~f:(fun board sidebar -> board ^ "  " ^ sidebar ^ "\n")

let render (game : Game.t) =
  let board_lines =
    List.map (List.range Board.first_row (Board.last_row + 1)) ~f:(render_row game)
    @ [ border ]
  in
  "\n" ^ String.concat ~sep:"" (combine_lines board_lines (sidebar_lines game))
