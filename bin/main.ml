open Core
open Async

let tick_interval = Time_float.Span.of_sec 0.5

let enter_game_screen () = printf "\027[?1049h\027[?25l%!"
let leave_game_screen () = Stdlib.output_string Stdlib.stdout "\027[?25h\027[?1049l"

let redraw game = printf "\027[H\027[J%s%!" (Tetris.Renderer.render game)

let rec run game =
  redraw game;
  Deferred.bind (Clock.after tick_interval) ~f:(fun () ->
    run (Tetris.Game.apply_action game Tetris.Types.Tick))

let () =
  Stdlib.at_exit leave_game_screen;
  never_returns
    (Scheduler.go_main ~main:(fun () ->
       enter_game_screen ();
       don't_wait_for (run (Tetris.Game.create ())))
       ())
