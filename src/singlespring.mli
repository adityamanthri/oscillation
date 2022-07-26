type t

type pos = float

type vel = float

val create_state : float -> float -> int -> t

val get_pos : t -> pos

val get_vel : t -> vel

val get_index : t -> int

val cur_accel : vel -> vel -> vel -> vel -> vel -> vel

val nextpos : t -> vel -> vel

val nextvel : t -> vel -> vel -> vel -> vel -> vel -> vel

val next_state : t -> vel -> vel -> vel -> vel -> vel -> t

val start :
  t -> vel -> vel -> vel -> vel -> vel -> int -> t list -> t list
