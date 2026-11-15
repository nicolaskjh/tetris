open Base

let test_next_piece_uses_its_tetromino_shape () =
  assert (List.equal String.equal (Tetris.Renderer.render_next_piece Tetris.Types.I) [ "████████"; "        " ]);
  assert (List.equal String.equal (Tetris.Renderer.render_next_piece Tetris.Types.T) [ "██████  "; "  ██    " ])

let test_render_shows_the_active_piece_and_sidebar () =
  let game = Tetris.Game.create ~first_piece:Tetris.Types.T ~next_piece:Tetris.Types.I () in
  let output = Tetris.Renderer.render game in
  assert (String.is_prefix output ~prefix:"\n");
  assert (String.is_substring output ~substring:"██████");
  assert (String.is_substring output ~substring:"░░░░");
  assert (String.is_substring output ~substring:"Score: 0");
  assert (String.is_substring output ~substring:"Lines: 0");
  assert (String.is_substring output ~substring:"░░░░")

let test_render_shows_game_over_banner_only_when_needed () =
  let game = Tetris.Game.create () in
  assert (not (String.is_substring (Tetris.Renderer.render game) ~substring:"GAME OVER"));
  let game = { game with phase = Tetris.Types.Game_over } in
  assert (String.is_substring (Tetris.Renderer.render game) ~substring:"| GAME OVER |")

let test_render_overlays_the_active_piece_on_the_fixed_board () =
  let game = Tetris.Game.create ~first_piece:Tetris.Types.O ~next_piece:Tetris.Types.I () in
  let game = Tetris.Game.apply_action game Tetris.Types.Hard_drop in
  let output = Tetris.Renderer.render game in
  assert (String.is_substring output ~substring:"████");
  assert (String.is_substring output ~substring:"░░░░░░░░")

let () =
  test_next_piece_uses_its_tetromino_shape ();
  test_render_shows_the_active_piece_and_sidebar ();
  test_render_shows_game_over_banner_only_when_needed ();
  test_render_overlays_the_active_piece_on_the_fixed_board ()
