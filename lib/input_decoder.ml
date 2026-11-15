open Controls

type state =
  | Initial
  | Escape
  | Arrow

type result =
  | Command of command
  | Ignored
  | Awaiting_input of state

let consume state character =
  match state, character with
  | Initial, '\027' -> Awaiting_input Escape
  | Initial, character ->
      (match command_of_char character with
       | Some command -> Command command
       | None -> Ignored)
  | Escape, '[' -> Awaiting_input Arrow
  | Escape, _ -> Ignored
  | Arrow, character ->
      (match command_of_arrow character with
       | Some command -> Command command
       | None -> Ignored)

let timeout = function
  | Escape -> Command Quit
  | Initial | Arrow -> Ignored
