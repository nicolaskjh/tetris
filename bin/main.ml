open Core
open Async

let tick_interval = Time_float.Span.of_sec 0.5
let saved_terminal_settings = ref None

let enter_game_screen () = printf "\027[?1049h\027[?25l%!"
let leave_game_screen () = Stdlib.output_string Stdlib.stdout "\027[?25h\027[?1049l"

let enable_raw_input () =
  if Core_unix.isatty Core_unix.stdin then (
    let original = Core_unix.Terminal_io.tcgetattr Core_unix.stdin in
    saved_terminal_settings := Some original;
    let raw = { original with c_icanon = false; c_echo = false; c_vmin = 1; c_vtime = 0 } in
    Core_unix.Terminal_io.tcsetattr raw Core_unix.stdin ~mode:Core_unix.Terminal_io.TCSANOW)

let restore_terminal () =
  match !saved_terminal_settings with
  | None -> ()
  | Some settings ->
      Core_unix.Terminal_io.tcsetattr settings Core_unix.stdin ~mode:Core_unix.Terminal_io.TCSANOW

let redraw game = printf "\027[H\027[J%s%!" (Tetris.Renderer.render game)

let apply_action game action =
  game := Tetris.Game.apply_action !game action;
  redraw !game

let rec tick_loop game =
  Deferred.bind (Clock.after tick_interval) ~f:(fun () ->
    apply_action game Tetris.Types.Tick;
    tick_loop game)

let read_arrow_command () =
  Deferred.bind (Reader.read_char (Lazy.force Reader.stdin)) ~f:(function
    | `Ok '[' ->
        Deferred.map (Reader.read_char (Lazy.force Reader.stdin)) ~f:(function
          | `Ok arrow -> Tetris.Controls.command_of_arrow arrow
          | `Eof -> None)
    | `Ok _ | `Eof -> Deferred.return None)

let rec input_loop game =
  Deferred.bind (Reader.read_char (Lazy.force Reader.stdin)) ~f:(function
    | `Eof -> Deferred.unit
    | `Ok '\027' ->
        Deferred.bind (read_arrow_command ()) ~f:(fun command ->
          (match command with
           | Some (Game_action action) -> apply_action game action
           | Some Quit -> Stdlib.exit 0
           | None -> ());
          input_loop game)
    | `Ok char ->
        (match Tetris.Controls.command_of_char char with
         | Some (Game_action action) -> apply_action game action
         | Some Quit -> Stdlib.exit 0
         | None -> ());
        input_loop game)

let () =
  Stdlib.at_exit (fun () ->
    restore_terminal ();
    leave_game_screen ());
  never_returns
    (Scheduler.go_main ~main:(fun () ->
       let game = ref (Tetris.Game.create ()) in
       enable_raw_input ();
       enter_game_screen ();
       redraw !game;
       don't_wait_for (tick_loop game);
       don't_wait_for (input_loop game))
       ())
