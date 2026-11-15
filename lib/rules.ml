type t =
  { gravity_interval_seconds : float
  ; lock_delay_seconds : float
  ; max_lock_resets : int
  }

let default =
  { gravity_interval_seconds = 0.5
  ; lock_delay_seconds = 1.0
  ; max_lock_resets = 15
  }
