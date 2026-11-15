type state =
  | Initial
  | Escape
  | Arrow

type result =
  | Command of Controls.command
  | Ignored
  | Awaiting_input of state

val consume : state -> char -> result
val timeout : state -> result
