type pos = float

type vel = float

type state = {
  position : pos;
  velocity : vel;
  index : int;
}

let cur_accel pos m k l g = g +. ((0. -. (k /. m)) *. (pos -. l))

let nextpos (prev : state) (dt : float) : pos =
  prev.position +. (dt *. prev.velocity)

let nextvel (prev : state) (dt : float) m k l g =
  prev.velocity +. (dt *. cur_accel prev.position m k l g)

let next_state (prev : state) (dt : float) m k l g =
  {
    position = nextpos prev dt;
    velocity = nextvel prev dt m k l g;
    index = prev.index + 1;
  }

(*Find way to print stuff out in this function; this is for the single
  spring*)
let rec start (current : state) dt m k l g datapoints acc =
  if current.index > datapoints then acc
  else
    start
      (next_state current dt m k l g)
      dt m k l g datapoints (current :: acc)

type element = {
  mass : float;
  spring_constant : float;
  resting_length : float;
  position : float;
  velocity : float;
  id : int;
}
(**Denotes a single unit of the chained spring: the spring constant and
   resting length of the spring above and the mass, along with the
   position and velocity of the mass and the id of the mass*)

type vertical_chain_state = {
  masses : element list;
  gravity : float;
  index : int;
  num_masses : int;
}

let calc_accel_first
    (firstunit : element)
    (secondunit : element)
    (g : float) =
  try
    g
    +. (secondunit.spring_constant
        *. (secondunit.position -. firstunit.position
          -. secondunit.resting_length)
       -. firstunit.spring_constant
          *. (firstunit.position -. firstunit.resting_length))
       /. firstunit.mass
  with
  | Not_found ->
      cur_accel firstunit.position firstunit.mass
        firstunit.spring_constant firstunit.resting_length g

let calc_accel_last (stlunit : element) (lastunit : element) (g : float)
    =
  try
    g
    -. lastunit.spring_constant
       *. (lastunit.position -. stlunit.position
         -. lastunit.resting_length)
       /. lastunit.mass
  with
  | Not_found ->
      cur_accel lastunit.position lastunit.mass lastunit.spring_constant
        lastunit.resting_length g

let calc_accel_middle
    (prevunit : element)
    (curunit : element)
    (nextunit : element)
    (g : float) =
  try
    g
    -. (curunit.spring_constant
        *. (curunit.position -. prevunit.position
          -. curunit.resting_length)
       +. nextunit.spring_constant
          *. (nextunit.position -. curunit.position
            -. nextunit.resting_length))
       /. curunit.mass
  with
  | Not_found -> calc_accel_last prevunit curunit g

let valnext pos vel dt = pos +. (vel *. dt)

let rec next_element_list
    (elements : element list)
    (previous : element)
    (g : float)
    acc
    num_masses
    dt : element list =
  match elements with
  | [] -> List.rev acc
  | h :: t ->
      if h.id = 1 then
        next_element_list t h g
          ({
             mass = h.mass;
             spring_constant = h.spring_constant;
             resting_length = h.resting_length;
             position = valnext h.position h.velocity dt;
             velocity =
               valnext h.velocity
                 (try
                    calc_accel_first h
                      (match t with
                      | [] -> raise Not_found
                      | h :: t -> h)
                      g
                  with
                 | Not_found ->
                     cur_accel h.position h.mass h.spring_constant
                       h.resting_length g)
                 dt;
             id = h.id;
           }
          :: acc)
          num_masses dt
      else if h.id = num_masses then
        next_element_list t h g
          ({
             mass = h.mass;
             spring_constant = h.spring_constant;
             resting_length = h.resting_length;
             position = valnext h.position h.velocity dt;
             velocity =
               valnext h.velocity (calc_accel_last previous h g) dt;
             id = h.id;
           }
          :: acc)
          num_masses dt
      else
        next_element_list t h g
          ({
             mass = h.mass;
             spring_constant = h.spring_constant;
             resting_length = h.resting_length;
             position = valnext h.position h.velocity dt;
             velocity =
               valnext h.velocity
                 (try
                    calc_accel_middle previous h
                      (match t with
                      | [] -> raise Not_found
                      | h :: t -> h)
                      g
                  with
                 | Not_found ->
                     cur_accel h.position h.mass h.spring_constant
                       h.resting_length g)
                 dt;
             id = h.id;
           }
          :: acc)
          num_masses dt

(*Once again, find a way to print the stuff out here*)
let rec start_chained_spring
    (current : vertical_chain_state)
    dt
    datapoints
    acc =
  if current.index > datapoints then acc
  else
    start_chained_spring
      {
        masses =
          next_element_list current.masses
            {
              mass = -1.;
              spring_constant = -1.;
              resting_length = -1.;
              position = -1.;
              velocity = -1.;
              id = 0;
            }
            current.gravity [] current.num_masses dt;
        gravity = current.gravity;
        index = current.index + 1;
        num_masses = current.num_masses;
      }
      dt datapoints (current :: acc)

(*2D stuff here*)
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

(*calculates the acceration using position, assuming the spring is fixed
  at the origin with gravity = g, resting length = l, mass = m and k is
  the spring constant. For simplicity assume the spring is able to
  collapse in on itself*)

let calc_accel_2d (position : vector) k l g m =
  add
    (scale
       (unit_direction (scale position (-1.0)))
       (k *. ((magnitude position -. l) /. m)))
    { x = 0.0; y = g }

type state_2d = {
  position : vector;
  velocity : vector;
}

(**A single 2d state, contains a position and velocity vector,
   acceleration will be calculated from the position*)
let zero_state =
  { position = { x = 0.; y = 0. }; velocity = { x = 0.; y = 0. } }

(**Given a current 2d state, computes the next one via numerical
   integration*)
let compute_next_2d_state (current : state_2d) k m l g dt =
  {
    position = increment current.position current.velocity dt;
    velocity =
      increment current.velocity
        (calc_accel_2d current.position k l g m)
        dt;
  }

(**Generates List of 2d states, reverses at the end to have them in the
   correct time order*)
let rec generate_states
    (current : state_2d)
    k
    m
    l
    g
    dt
    index
    datapoints
    acc =
  if index > datapoints then List.rev acc
  else
    generate_states
      (compute_next_2d_state current k m l g dt)
      k m l g dt (index + 1) datapoints (current :: acc)

type element_2d = {
  motion_vec : state_2d;
  resting_length : float;
  mass : float;
  spring_cons : float;
}

let zero_elem_2d =
  {
    motion_vec = zero_state;
    resting_length = 0.;
    mass = 0.;
    spring_cons = 0.;
  }

type chained_2d_state = {
  masses : element_2d list;
  num_mass : int;
  trial_num : int;
}

let calc_2d_accel
    (prev : element_2d)
    (cur : element_2d)
    (after : element_2d)
    no_after
    g =
  add { x = 0.; y = g }
    (add
       (scale
          (unit_direction
             (add cur.motion_vec.position
                (scale prev.motion_vec.position (-1.))))
          (cur.spring_cons
          *. (magnitude
                (add cur.motion_vec.position
                   (scale prev.motion_vec.position (-1.)))
             -. cur.resting_length)
          /. cur.mass))
       (if no_after then { x = 0.0; y = 0.0 }
       else
         scale
           (unit_direction
              (add cur.motion_vec.position
                 (scale after.motion_vec.position (-1.))))
           (after.spring_cons
           *. (magnitude
                 (add cur.motion_vec.position
                    (scale after.motion_vec.position (-1.)))
              -. after.resting_length)
           /. cur.mass)))

let accel_2d_list (current_config : element_2d list) (first : bool) g =
  let accels = ref [] in
  for i = 0 to List.length current_config - 1 do
    let prev =
      if i = 0 then zero_elem_2d else List.nth current_config (i - 1)
    in
    let cur = List.nth current_config i in
    let next =
      if i = List.length current_config - 1 then zero_elem_2d
      else List.nth current_config (i + 1)
    in
    accels :=
      calc_2d_accel prev cur next (i < List.length current_config - 1) g
      :: !accels
  done;
  List.rev !accels

let update_element (elt : element_2d) (acceleration : vector) dt :
    element_2d =
  {
    motion_vec =
      {
        position =
          increment elt.motion_vec.position elt.motion_vec.velocity dt;
        velocity = increment elt.motion_vec.velocity acceleration dt;
      };
    resting_length = elt.resting_length;
    mass = elt.mass;
    spring_cons = elt.spring_cons;
  }

let next_chained_2d_state
    (current : chained_2d_state)
    (accelerations : vector list)
    dt =
  {
    masses =
      (let acc = ref [] in
       for i = 0 to List.length current.masses - 1 do
         acc :=
           update_element
             (List.nth current.masses i)
             (List.nth accelerations i)
             dt
           :: !acc
       done;
       List.rev !acc);
    num_mass = current.num_mass;
    trial_num = current.trial_num + 1;
  }

let rec generate_chained_2d_states
    (cur : chained_2d_state)
    g
    dt
    datapoints
    acc =
  if cur >= datapoints then List.rev acc
  else
    let next_state =
      next_chained_2d_state cur (accel_2d_list cur.masses true g) dt
    in
    generate_chained_2d_states next_state g dt datapoints (cur :: acc)
