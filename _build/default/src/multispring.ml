open Singlespring

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

type t = {
  masses : element list;
  gravity : float;
  index : int;
  num_masses : int;
}

let get_masses state = state.masses

let create_state elt_lst g ind num_mass =
  { masses = elt_lst; gravity = g; index = ind; num_masses = num_mass }

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
                      | h :: _ -> h)
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
                      | h :: _ -> h)
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

let rec start_chained_spring (current : t) dt datapoints acc =
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
