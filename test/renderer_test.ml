open Base

let empty_row = "|          |\n"

let test_render_shows_the_active_piece_in_the_spawn_buffer () =
  let game = Tetris.Game.create ~first_piece:Tetris.Types.T ~next_piece:Tetris.Types.I () in
  let expected =
    String.concat ~sep:""
      ([ "          \n"
       ; "          \n"
       ; "   ###    \n"
       ; "    #     \n"
       ; empty_row
       ]
       @ List.init 19 ~f:(fun _ -> empty_row)
       @ [ "+----------+\n"; "Score: 0  Lines: 0\n"; "Next: I  Playing\n" ])
  in
  assert (String.equal (Tetris.Renderer.render game) expected)

let test_render_overlays_the_active_piece_on_the_fixed_board () =
  let game = Tetris.Game.create ~first_piece:Tetris.Types.O ~next_piece:Tetris.Types.I () in
  let game = Tetris.Game.apply_action game Tetris.Types.Hard_drop in
  let output = Tetris.Renderer.render game in
  assert (String.is_substring output ~substring:"   ####   \n");
  assert (String.is_substring output ~substring:"|    ##    |")

let () =
  test_render_shows_the_active_piece_in_the_spawn_buffer ();
  test_render_overlays_the_active_piece_on_the_fixed_board ()
