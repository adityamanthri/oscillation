type pos = float

type vel = float

type t = {
  position : pos;
  velocity : vel;
  index : int;
}

let create_state pos vel ind =
  { position = pos; velocity = vel; index = ind }

let get_pos state = state.position

let get_vel state = state.velocity

let get_index state = state.index

let cur_accel pos m k l g = g +. ((0. -. (k /. m)) *. (pos -. l))

let nextpos (prev : t) (dt : float) : pos =
  prev.position +. (dt *. prev.velocity)

let nextvel (prev : t) (dt : float) m k l g =
  prev.velocity +. (dt *. cur_accel prev.position m k l g)

let next_state (prev : t) (dt : float) m k l g =
  {
    position = nextpos prev dt;
    velocity = nextvel prev dt m k l g;
    index = prev.index + 1;
  }

let rec start (current : t) dt m k l g datapoints acc =
  if current.index > datapoints then acc
  else
    start
      (next_state current dt m k l g)
      dt m k l g datapoints (current :: acc)
