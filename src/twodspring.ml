type pos = float

type vel = float

type vector = {
  x : float;
  y : float;
}

let scale (v : vector) (s : float) : vector =
  { x = v.x *. s; y = v.y *. s }

let add (v : vector) (u : vector) : vector =
  { x = v.x +. u.x; y = v.y +. u.y }

let magnitude (v : vector) = Float.sqrt ((v.x *. v.x) +. (v.y *. v.y))

let increment (v : vector) (change : vector) dt =
  add v (scale change dt)

let unit_direction (v : vector) : vector = scale v (1.0 /. magnitude v)

let calc_accel_2d (position : vector) k l g m =
  add
    (scale
       (unit_direction (scale position (-1.0)))
       (k *. ((magnitude position -. l) /. m)))
    { x = 0.0; y = g }

type t = {
  position : vector;
  velocity : vector;
}

let get_pos state = state.position

let get_posx state = state.position.x

let get_posy state = state.position.y

let get_vel state = state.velocity

let get_velx state = state.velocity.x

let get_vely state = state.velocity.y

let make_state x1 y1 x2 y2 =
  { position = { x = x1; y = y1 }; velocity = { x = x2; y = y2 } }

let compute_next_2d_state (current : t) k m l g dt =
  {
    position = increment current.position current.velocity dt;
    velocity =
      increment current.velocity
        (calc_accel_2d current.position k l g m)
        dt;
  }

let rec generate_states (current : t) k m l g dt index datapoints acc =
  if index > datapoints then List.rev acc
  else
    generate_states
      (compute_next_2d_state current k m l g dt)
      k m l g dt (index + 1) datapoints (current :: acc)