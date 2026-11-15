open Base
open Stdio

let () =
  let game = Tetris.Game.create () in
  printf "%s" (Tetris.Renderer.render game)
