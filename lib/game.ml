open Base
open Types

type t =
  { board : Board.t
  ; active_piece : Piece.t
  ; next_piece : piece_kind
  ; randomizer : Randomizer.t
  ; score : int
  ; lines_cleared : int
  ; phase : phase
  }

let create ?(seed = 1) ?first_piece ?next_piece () =
  let randomizer = Randomizer.create ~seed in
  let generated_first_piece, randomizer = Randomizer.next randomizer in
  let generated_next_piece, randomizer = Randomizer.next randomizer in
  let first_piece = Option.value first_piece ~default:generated_first_piece in
  let next_piece = Option.value next_piece ~default:generated_next_piece in
  { board = Board.empty ()
  ; active_piece = Piece.spawn first_piece
  ; next_piece
  ; randomizer
  ; score = 0
  ; lines_cleared = 0
  ; phase = Playing
  }

let try_move state ~columns ~rows =
  let active_piece = Piece.move state.active_piece ~columns ~rows in
  if Board.can_place state.board active_piece then { state with active_piece } else state

let try_rotate state =
  let active_piece = Piece.rotate_clockwise state.active_piece in
  if Board.can_place state.board active_piece then { state with active_piece } else state

let points_for_lines = function
  | 1 -> 100
  | 2 -> 300
  | 3 -> 500
  | 4 -> 800
  | _ -> 0

let lock_piece state =
  let board = Board.place state.board state.active_piece in
  let board, cleared_lines = Board.clear_complete_lines board in
  let next_piece, randomizer = Randomizer.next state.randomizer in
  let active_piece = Piece.spawn state.next_piece in
  let phase = if Board.can_place board active_piece then Playing else Game_over in
  { board
  ; active_piece
  ; next_piece
  ; randomizer
  ; score = state.score + points_for_lines cleared_lines
  ; lines_cleared = state.lines_cleared + cleared_lines
  ; phase
  }

let move_down_or_lock state =
  let moved = try_move state ~columns:0 ~rows:1 in
  if phys_equal moved state then lock_piece state else moved

let apply_action state action =
  match state.phase with
  | Game_over -> state
  | Playing ->
      (match action with
       | Move_left -> try_move state ~columns:(-1) ~rows:0
       | Move_right -> try_move state ~columns:1 ~rows:0
       | Rotate_clockwise -> try_rotate state
       | Soft_drop | Tick -> move_down_or_lock state
       | Hard_drop ->
      let rec fall state =
        let next = try_move state ~columns:0 ~rows:1 in
        if phys_equal next state then state else fall next
      in
      lock_piece (fall state))
