open Types

type command =
  | Game_action of action
  | Quit

let command_of_char = function
  | 'a' -> Some (Game_action Move_left)
  | 'd' -> Some (Game_action Move_right)
  | 'w' -> Some (Game_action Rotate_clockwise)
  | 'z' -> Some (Game_action Rotate_counterclockwise)
  | 's' -> Some (Game_action Soft_drop)
  | ' ' -> Some (Game_action Hard_drop)
  | 'c' -> Some (Game_action Hold)
  | _ -> None

let command_of_arrow = function
  | 'A' -> Some (Game_action Rotate_clockwise)
  | 'B' -> Some (Game_action Soft_drop)
  | 'C' -> Some (Game_action Move_right)
  | 'D' -> Some (Game_action Move_left)
  | _ -> None
