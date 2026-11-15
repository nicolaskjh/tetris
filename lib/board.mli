open Types

type t

val width : int
val hidden_rows : int
val visible_height : int
val first_row : int
val last_row : int

val empty : unit -> t
val with_filled_cells : coordinate list -> t
val cell : t -> coordinate -> cell
val is_empty : t -> coordinate -> bool
val can_place : t -> Piece.t -> bool
val place : t -> Piece.t -> t
val clear_complete_lines : t -> t * int
