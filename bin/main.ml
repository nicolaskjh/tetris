open Base
open Stdio

let () =
  let game = Tetris.Game.create () in
  printf "Tetris scaffold ready: level %d, score %d.\n" game.level game.score;