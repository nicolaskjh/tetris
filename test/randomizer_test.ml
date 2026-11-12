open Base
open Tetris.Types

let same_piece left right =
  match left, right with
  | I, I
  | O, O
  | T, T
  | J, J
  | L, L
  | S, S
  | Z, Z -> true
  | _ -> false

let rec take randomizer count =
  if count = 0 then [], randomizer
  else
    let piece, randomizer = Tetris.Randomizer.next randomizer in
    let remaining, randomizer = take randomizer (count - 1) in
    piece :: remaining, randomizer

let count_piece pieces target = List.count pieces ~f:(fun piece -> same_piece piece target)

let test_every_bag_contains_each_piece_once () =
  let pieces, _ = take (Tetris.Randomizer.create ~seed:42) 7 in
  List.iter Tetris.Randomizer.all_piece_kinds ~f:(fun piece ->
    assert (count_piece pieces piece = 1))

let test_seeded_randomizers_are_repeatable () =
  let first, _ = take (Tetris.Randomizer.create ~seed:73) 21 in
  let second, _ = take (Tetris.Randomizer.create ~seed:73) 21 in
  assert (List.equal same_piece first second)

let () =
  test_every_bag_contains_each_piece_once ();
  test_seeded_randomizers_are_repeatable ()
