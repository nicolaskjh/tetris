# Tetris

A terminal Tetris learning project written in OCaml with Dune, `Base`, and `Async`.

## Run

From this directory:

```sh
opam exec -- dune exec bin/main.exe
```

The game runs in an alternate terminal screen. Press Esc to leave it.

## Controls

| Input | Action |
| --- | --- |
| Left / `A` | Move left |
| Right / `D` | Move right |
| Up / `W` | Rotate clockwise |
| Down / `S` | Soft drop |
| Space | Hard drop |
| Esc | Quit |

## Current mechanics

- 10×20 visible playfield with a spawn buffer
- Seven-bag, seeded piece randomizer
- Collision detection, hard and soft drop, locking, line clears, and scoring

## Test and build

```sh
opam exec -- dune build
opam exec -- dune runtest
```

## Layout

- `lib/` - game rules, board, pieces, randomizer, controls, and rendering
- `bin/` - Async game loop and terminal I/O
- `test/` - focused tests for each module
