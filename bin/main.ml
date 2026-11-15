open Core
open Async

let tick_interval = Time_float.Span.of_sec 0.5

let apply_action game action =
  game := Tetris.Game.apply_action !game action;
  Terminal.redraw !game

let rec tick_loop game =
  Deferred.bind (Clock.after tick_interval) ~f:(fun () ->
    apply_action game Tetris.Types.Tick;
    tick_loop game)

let rec input_loop terminal game =
  Deferred.bind (Terminal.read_input terminal) ~f:(function
    | Terminal.End_of_input -> Deferred.unit
    | Terminal.Ignored -> input_loop terminal game
    | Terminal.Command (Game_action action) ->
        apply_action game action;
        input_loop terminal game
    | Terminal.Command Quit -> Stdlib.exit 0)

let () =
  let terminal = Terminal.create () in
  Stdlib.at_exit (fun () ->
    Terminal.restore terminal;
    Terminal.leave_game_screen ());
  never_returns
    (Scheduler.go_main ~main:(fun () ->
       let game = ref (Tetris.Game.create ()) in
       Terminal.enable_raw_input terminal;
       Terminal.enter_game_screen ();
       Terminal.redraw !game;
       Terminal.start_input terminal;
       don't_wait_for (tick_loop game);
       don't_wait_for (input_loop terminal game))
       ())
