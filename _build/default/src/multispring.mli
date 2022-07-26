type pos = float

type vel = float

type element = {
  mass : float;
  spring_constant : float;
  resting_length : float;
  position : float;
  velocity : float;
  id : int;
}

type t

val get_masses : t -> element list

val create_state : element list -> float -> int -> int -> t

val calc_accel_first : element -> element -> vel -> vel

val calc_accel_last : element -> element -> vel -> vel

val calc_accel_middle : element -> element -> element -> vel -> vel

val valnext : vel -> vel -> vel -> vel

val next_element_list :
  element list ->
  element ->
  vel ->
  element list ->
  int ->
  vel ->
  element list

val start_chained_spring : t -> vel -> int -> t list -> t list