open Base
open Types

type t = cell array array

let width = 10
let hidden_rows = 2
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
