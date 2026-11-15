open Core
open Async

type t =
  { mutable saved_settings : Core_unix.Terminal_io.t option
  ; input : char Pipe.Reader.t
  ; input_writer : char Pipe.Writer.t
  ; mutable input_started : bool
  }

type input_result =
  | Command of Tetris.Controls.command
  | Ignored
  | End_of_input

let escape_timeout = Time_float.Span.of_sec 0.05

let create () =
  let input, input_writer = Pipe.create () in
  { saved_settings = None; input; input_writer; input_started = false }

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

let rec copy_input_to_pipe terminal =
  Deferred.bind (Reader.read_char (Lazy.force Reader.stdin)) ~f:(function
    | `Eof ->
        Pipe.close terminal.input_writer;
        Deferred.unit
    | `Ok character ->
        Deferred.bind (Pipe.write terminal.input_writer character) ~f:(fun () ->
          copy_input_to_pipe terminal))

let start_input terminal =
  if not terminal.input_started
  then (
    terminal.input_started <- true;
    don't_wait_for (copy_input_to_pipe terminal))

let read_character terminal = Pipe.read terminal.input

let rec decode_character terminal state character =
  match Tetris.Input_decoder.consume state character with
  | Tetris.Input_decoder.Command command -> Deferred.return (Command command)
  | Tetris.Input_decoder.Ignored -> Deferred.return Ignored
  | Tetris.Input_decoder.Awaiting_input Tetris.Input_decoder.Escape -> read_escape_input terminal
  | Tetris.Input_decoder.Awaiting_input state -> read_sequence_character terminal state

and read_escape_input terminal =
  Deferred.bind
    (Clock.with_timeout escape_timeout (read_character terminal))
    ~f:(function
      | `Timeout ->
          (match Tetris.Input_decoder.timeout Tetris.Input_decoder.Escape with
           | Tetris.Input_decoder.Command command -> Deferred.return (Command command)
           | Tetris.Input_decoder.Ignored | Tetris.Input_decoder.Awaiting_input _ -> assert false)
      | `Result `Eof -> Deferred.return End_of_input
      | `Result (`Ok character) -> decode_character terminal Tetris.Input_decoder.Escape character)

and read_sequence_character terminal state =
  Deferred.bind (read_character terminal) ~f:(function
    | `Eof -> Deferred.return End_of_input
    | `Ok character -> decode_character terminal state character)

let read_input terminal =
  Deferred.bind (read_character terminal) ~f:(function
    | `Eof -> Deferred.return End_of_input
    | `Ok character -> decode_character terminal Tetris.Input_decoder.Initial character)
