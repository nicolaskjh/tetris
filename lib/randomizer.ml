open Base
open Types

type t =
  { seed : int
  ; bag : piece_kind list
  }

let all_piece_kinds = [ I; O; T; J; L; S; Z ]
let modulus = 2_147_483_647

let create ~seed = { seed = Int.rem seed modulus; bag = [] }

let next_seed seed = Int.rem ((seed * 1_103_515_245) + 12_345) modulus

let shuffle seed =
  let pieces = Array.of_list all_piece_kinds in
  let seed = ref seed in
  for index = Array.length pieces - 1 downto 1 do
    seed := next_seed !seed;
    let other_index = Int.rem !seed (index + 1) in
    let piece = pieces.(index) in
    pieces.(index) <- pieces.(other_index);
    pieces.(other_index) <- piece
  done;
  Array.to_list pieces, !seed

let refill randomizer =
  match randomizer.bag with
  | [] ->
      let bag, seed = shuffle randomizer.seed in
      { seed; bag }
  | _ -> randomizer

let next randomizer =
  let randomizer =
    match randomizer.bag with
    | [] -> refill randomizer
    | _ -> randomizer
  in
  match randomizer.bag with
  | piece :: bag -> piece, { randomizer with bag }
  | [] -> assert false
