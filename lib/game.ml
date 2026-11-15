open Base
open Types

type t =
  { board : Board.t
  ; active_piece : Piece.t
  ; held_piece : piece_kind option
  ; can_hold : bool
  ; next_piece : piece_kind
  ; randomizer : Randomizer.t
  ; score : int
  ; lines_cleared : int
  ; grounded_ticks : int
  ; phase : phase
  }

let lock_delay_ticks = 2

let create ?(seed = 1) ?first_piece ?next_piece () =
  let randomizer = Randomizer.create ~seed in
  let generated_first_piece, randomizer = Randomizer.next randomizer in
  let generated_next_piece, randomizer = Randomizer.next randomizer in
  let first_piece = Option.value first_piece ~default:generated_first_piece in
  let next_piece = Option.value next_piece ~default:generated_next_piece in
  { board = Board.empty ()
  ; active_piece = Piece.spawn first_piece
  ; held_piece = None
  ; can_hold = true
  ; next_piece
  ; randomizer
  ; score = 0
  ; lines_cleared = 0
  ; grounded_ticks = 0
  ; phase = Playing
  }

let try_move state ~columns ~rows =
  let active_piece = Piece.move state.active_piece ~columns ~rows in
  if Board.can_place state.board active_piece then Some { state with active_piece } else None

let reset_lock_delay state = { state with grounded_ticks = 0 }

let move_if_possible state ~columns ~rows =
  match try_move state ~columns ~rows with
  | Some moved -> reset_lock_delay moved
  | None -> state

let try_rotate state =
  let active_piece = Piece.rotate_clockwise state.active_piece in
  if Board.can_place state.board active_piece then Some { state with active_piece } else None

let rotate_if_possible state =
  match try_rotate state with
  | Some rotated -> reset_lock_delay rotated
  | None -> state

let points_for_lines = function
  | 1 -> 100
  | 2 -> 300
  | 3 -> 500
  | 4 -> 800
  | _ -> 0

let spawn_piece state kind =
  let active_piece = Piece.spawn kind in
  let phase = if Board.can_place state.board active_piece then Playing else Game_over in
  { state with active_piece; grounded_ticks = 0; phase }

let lock_piece state =
  let board = Board.place state.board state.active_piece in
  let board, cleared_lines = Board.clear_complete_lines board in
  let active_kind = state.next_piece in
  let next_piece, randomizer = Randomizer.next state.randomizer in
  let state =
    { state with
      board
    ; next_piece
    ; randomizer
    ; score = state.score + points_for_lines cleared_lines
    ; lines_cleared = state.lines_cleared + cleared_lines
    ; can_hold = true
    }
  in
  spawn_piece state active_kind

let hold_piece state =
  if not state.can_hold then state
  else
    match state.held_piece with
    | None ->
        let active_kind = state.next_piece in
        let next_piece, randomizer = Randomizer.next state.randomizer in
        let state =
          { state with
            held_piece = Some state.active_piece.kind
          ; can_hold = false
          ; next_piece
          ; randomizer
          }
        in
        spawn_piece state active_kind
    | Some held_piece ->
        let state =
          { state with held_piece = Some state.active_piece.kind; can_hold = false }
        in
        spawn_piece state held_piece

let move_down_or_lock state =
  match try_move state ~columns:0 ~rows:1 with
  | Some moved -> reset_lock_delay moved
  | None ->
      let grounded_ticks = state.grounded_ticks + 1 in
      if grounded_ticks >= lock_delay_ticks then lock_piece state else { state with grounded_ticks }

let apply_action state action =
  match state.phase with
  | Game_over -> state
  | Playing ->
      (match action with
       | Move_left -> move_if_possible state ~columns:(-1) ~rows:0
       | Move_right -> move_if_possible state ~columns:1 ~rows:0
       | Rotate_clockwise -> rotate_if_possible state
       | Soft_drop -> move_if_possible state ~columns:0 ~rows:1
       | Tick -> move_down_or_lock state
       | Hold -> hold_piece state
       | Hard_drop ->
      let rec fall state =
        match try_move state ~columns:0 ~rows:1 with
        | Some moved -> fall moved
        | None -> state
      in
      lock_piece (fall state))
