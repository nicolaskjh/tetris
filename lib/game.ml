open Base
open Types

type t =
  { board : Board.t
  ; active_piece : Piece.t
  ; next_piece : piece_kind
  ; score : int
  ; lines_cleared : int
  ; level : int
  ; phase : phase
  }

let create ?(first_piece = T) ?(next_piece = I) () =
  { board = Board.empty ()
  ; active_piece = Piece.spawn first_piece
  ; next_piece
  ; score = 0
  ; lines_cleared = 0
  ; level = 1
  ; phase = Playing
  }

let try_move state ~columns ~rows =
  let active_piece = Piece.move state.active_piece ~columns ~rows in
  if Board.can_place state.board active_piece then { state with active_piece } else state

let try_rotate state =
  let active_piece = Piece.rotate_clockwise state.active_piece in
  if Board.can_place state.board active_piece then { state with active_piece } else state

let apply_action state = function
  | Move_left -> try_move state ~columns:(-1) ~rows:0
  | Move_right -> try_move state ~columns:1 ~rows:0
  | Rotate_clockwise -> try_rotate state
  | Soft_drop | Tick -> try_move state ~columns:0 ~rows:1
  | Hard_drop ->
      let rec fall state =
        let next = try_move state ~columns:0 ~rows:1 in
        if phys_equal next state then state else fall next
      in
      fall state
