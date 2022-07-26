type pos = float
type vel = float

open Twodspring

type element_2d = {
  motion_vec : Twodspring.t;
  resting_length : float;
  mass : float;
  spring_cons : float;
}

type t

val get_masses : t -> element_2d list
val make_t : element_2d list -> int -> int -> t
val zero_elem_2d : element_2d
val scale : Twodspring.vector -> vel -> Twodspring.vector
val add : Twodspring.vector -> Twodspring.vector -> Twodspring.vector
val magnitude : Twodspring.vector -> vel

val increment :
  Twodspring.vector -> Twodspring.vector -> vel -> Twodspring.vector

val unit_direction : Twodspring.vector -> Twodspring.vector

val calc_2d_accel :
  element_2d ->
  element_2d ->
  element_2d ->
  bool ->
  bool ->
  vel ->
  Twodspring.vector

val accel_2d_list :
  element_2d list -> bool -> vel -> Twodspring.vector list

val update_element :
  element_2d -> Twodspring.vector -> vel -> element_2d

val create_state : element_2d list -> int -> int -> t
val next_chained_2d_state : t -> Twodspring.vector list -> vel -> t

val generate_chained_2d_states :
  t -> vel -> vel -> int -> t list -> t list

val make_ellist_from_coorlst :
  (Twodspring.pos * Twodspring.pos) list ->
  float ->
  float ->
  float ->
  element_2d list ->
  element_2d list

val generate_coordinate_list :
  element_2d list -> Twodspring.vector list -> Twodspring.vector list