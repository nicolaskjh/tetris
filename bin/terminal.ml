open Core
open Async

type t = { mutable saved_settings : Core_unix.Terminal_io.t option }

type input_result =
  | Command of Tetris.Controls.command
  | Ignored
  | End_of_input

let create () = { saved_settings = None }

let enter_game_screen () = printf "\027[?1049h\027[?25l%!"

let leave_game_screen () =
  Stdlib.output_string Stdlib.stdout "\027[?25h\027[?1049l";
  Stdlib.flush Stdlib.stdout

let enable_raw_input terminal =
  if Core_unix.isatty Core_unix.stdin then (
    let original = Core_unix.Terminal_io.tcgetattr Core_unix.stdin in
    terminal.saved_settings <- Some original;
    let raw = { original with c_icanon = false; c_echo = false; c_vmin = 1; c_vtime = 0 } in
    Core_unix.Terminal_io.tcsetattr raw Core_unix.stdin ~mode:Core_unix.Terminal_io.TCSANOW)

let restore terminal =
  match terminal.saved_settings with
  | None -> ()
  | Some settings ->
      Core_unix.Terminal_io.tcsetattr settings Core_unix.stdin ~mode:Core_unix.Terminal_io.TCSANOW

let redraw game = printf "\027[H\027[J%s%!" (Tetris.Renderer.render game)

let read_arrow_input () =
  Deferred.bind (Reader.read_char (Lazy.force Reader.stdin)) ~f:(function
    | `Eof -> Deferred.return End_of_input
    | `Ok '[' ->
        Deferred.map (Reader.read_char (Lazy.force Reader.stdin)) ~f:(function
          | `Eof -> End_of_input
          | `Ok arrow ->
              (match Tetris.Controls.command_of_arrow arrow with
               | Some command -> Command command
               | None -> Ignored))
    | `Ok _ -> Deferred.return Ignored)

let read_input () =
  Deferred.bind (Reader.read_char (Lazy.force Reader.stdin)) ~f:(function
    | `Eof -> Deferred.return End_of_input
    | `Ok '\027' -> read_arrow_input ()
    | `Ok character ->
        Deferred.return
          (match Tetris.Controls.command_of_char character with
           | Some command -> Command command
           | None -> Ignored))
