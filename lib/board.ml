open Base
open Types

type t = cell array array

let width = 10
let hidden_rows = 4
let visible_height = 20
let first_row = -hidden_rows
let last_row = visible_height - 1
let storage_height = hidden_rows + visible_height

let empty () = Array.init storage_height ~f:(fun _ -> Array.create ~len:width Empty)

let is_inside { column; row } = column >= 0 && column < width && row >= first_row && row <= last_row

let storage_row row = row + hidden_rows

let cell board { column; row } = board.(storage_row row).(column)

let is_empty board coordinate =
  match cell board coordinate with
  | Empty -> true
  | Filled -> false

let can_place board piece =
  List.for_all (Piece.squares piece) ~f:(fun coordinate -> is_inside coordinate && is_empty board coordinate)

let copy board = Array.init storage_height ~f:(fun row -> Array.copy board.(row))

let place board piece =
  let board = copy board in
  List.iter (Piece.squares piece) ~f:(fun ({ column; row } : coordinate) ->
    board.(storage_row row).(column) <- Filled);
  board

let row_is_full board row =
  List.for_all (List.range 0 width) ~f:(fun column ->
    match cell board { column; row } with
    | Empty -> false
    | Filled -> true)

let clear_complete_lines board =
  let rows_to_keep =
    List.filter (List.range first_row (last_row + 1)) ~f:(fun row ->
      row < 0 || not (row_is_full board row))
  in
  let cleared_lines = storage_height - List.length rows_to_keep in
  let cleared_board = empty () in
  let destination_row = ref last_row in
  List.iter (List.rev rows_to_keep) ~f:(fun source_row ->
    cleared_board.(storage_row !destination_row) <- Array.copy board.(storage_row source_row);
    destination_row := !destination_row - 1);
  cleared_board, cleared_lines
