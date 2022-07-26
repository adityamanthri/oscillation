type pos = float

type vel = float

type vector = {
  x : float;
  y : float;
}

val scale : vector -> vel -> vector

val add : vector -> vector -> vector

val magnitude : vector -> vel

val increment : vector -> vector -> vel -> vector

val unit_direction : vector -> vector

val calc_accel_2d : vector -> vel -> vel -> vel -> vel -> vector

type t

val get_pos : t -> vector

val get_posx : t -> vel

val get_posy : t -> vel

val get_vel : t -> vector

val get_velx : t -> vel

val get_vely : t -> vel

val make_state : pos -> pos -> pos -> pos -> t

val compute_next_2d_state : t -> vel -> vel -> vel -> vel -> vel -> t

val generate_states :
  t -> vel -> vel -> vel -> vel -> vel -> int -> int -> t list -> t list
