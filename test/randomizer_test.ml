open Base
open Tetris.Types

let rec take randomizer count =
  if count = 0 then [], randomizer
  else
    let piece, randomizer = Tetris.Randomizer.next randomizer in
    let remaining, randomizer = take randomizer (count - 1) in
    piece :: remaining, randomizer

let count_piece pieces target = List.count pieces ~f:(fun piece -> equal_piece_kind piece target)

let test_every_bag_contains_each_piece_once () =
  let pieces, _ = take (Tetris.Randomizer.create ~seed:42) 7 in
  List.iter Tetris.Randomizer.all_piece_kinds ~f:(fun piece ->
    assert (count_piece pieces piece = 1))

let test_seeded_randomizers_are_repeatable () =
  let first, _ = take (Tetris.Randomizer.create ~seed:73) 21 in
  let second, _ = take (Tetris.Randomizer.create ~seed:73) 21 in
  assert (List.equal equal_piece_kind first second)

let test_negative_seeds_are_normalized () =
  let negative_seed, _ = take (Tetris.Randomizer.create ~seed:(-1)) 21 in
  let normalized_seed, _ = take (Tetris.Randomizer.create ~seed:(Tetris.Randomizer.modulus - 1)) 21 in
  assert (List.equal equal_piece_kind negative_seed normalized_seed)

let () =
  test_every_bag_contains_each_piece_once ();
  test_seeded_randomizers_are_repeatable ();
  test_negative_seeds_are_normalized ()
