open Base
open Stdio

let () =
  let game = Tetris.Game.create () in
  printf "Tetris scaffold ready: score %d.\n" game.score;
