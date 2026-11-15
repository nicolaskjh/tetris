open Base
open Types

type lock_delay =
  { elapsed_seconds : float
  ; reset_count : int
  }

type t =
  { board : Board.t
  ; rules : Rules.t
  ; active_piece : Piece.t
  ; held_piece : piece_kind option
  ; can_hold : bool
  ; next_piece : piece_kind
  ; randomizer : Randomizer.t
  ; score : int
  ; lines_cleared : int
  ; lock_delay : lock_delay
  ; phase : phase
  }

let fresh_lock_delay = { elapsed_seconds = 0.0; reset_count = 0 }

let create ?(rules = Rules.default) ?(seed = 1) ?first_piece ?next_piece () =
  let randomizer = Randomizer.create ~seed in
  let generated_first_piece, randomizer = Randomizer.next randomizer in
  let generated_next_piece, randomizer = Randomizer.next randomizer in
  let first_piece = Option.value first_piece ~default:generated_first_piece in
  let next_piece = Option.value next_piece ~default:generated_next_piece in
  { board = Board.empty ()
  ; rules
  ; active_piece = Piece.spawn first_piece
  ; held_piece = None
  ; can_hold = true
  ; next_piece
  ; randomizer
  ; score = 0
  ; lines_cleared = 0
  ; lock_delay = fresh_lock_delay
  ; phase = Playing
  }

let try_move state ~columns ~rows =
  let active_piece = Piece.move state.active_piece ~columns ~rows in
  if Board.can_place state.board active_piece then Some { state with active_piece } else None

let gravity_interval_seconds state = state.rules.gravity_interval_seconds

let reset_lock_timer lock_delay = { lock_delay with elapsed_seconds = 0.0 }

let is_grounded state =
  let below = Piece.move state.active_piece ~columns:0 ~rows:1 in
  not (Board.can_place state.board below)

let reset_lock_delay_after_player_action state moved =
  if is_grounded state && state.lock_delay.reset_count < state.rules.max_lock_resets
  then
    { moved with
      lock_delay =
        { elapsed_seconds = 0.0
        ; reset_count = state.lock_delay.reset_count + 1
        }
    }
  else moved

let move_if_possible state ~columns ~rows =
  match try_move state ~columns ~rows with
  | Some moved -> reset_lock_delay_after_player_action state moved
  | None -> state

let try_rotate state =
  let rotated_piece = Piece.rotate_clockwise state.active_piece in
  List.find_map (Piece.clockwise_kicks state.active_piece) ~f:(fun kick ->
    let active_piece = Piece.move rotated_piece ~columns:kick.columns ~rows:kick.rows in
    if Board.can_place state.board active_piece then Some { state with active_piece } else None)

let rotate_if_possible state =
  match try_rotate state with
  | Some rotated -> reset_lock_delay_after_player_action state rotated
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
  { state with active_piece; lock_delay = fresh_lock_delay; phase }

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
  | Some moved -> { moved with lock_delay = reset_lock_timer moved.lock_delay }
  | None ->
      let lock_delay =
        { state.lock_delay with
          elapsed_seconds = state.lock_delay.elapsed_seconds +. state.rules.gravity_interval_seconds
        }
      in
      if Float.(lock_delay.elapsed_seconds >= state.rules.lock_delay_seconds)
      then lock_piece state
      else { state with lock_delay }

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
