type pos = float
type vel = float

open Twodspring

type vector = Twodspring.vector

let get_vec_x vector = vector.x
let get_vec_y vector = vector.y

type element_2d = {
  motion_vec : Twodspring.t;
  resting_length : float;
  mass : float;
  spring_cons : float;
}

type t = {
  masses : element_2d list;
  num_mass : int;
  trial_num : int;
}

let make_init_elt pos l m k =
  {
    motion_vec =
      (match pos with
      | x, y -> Twodspring.make_state x y 0.0 0.0);
    resting_length = l;
    mass = m;
    spring_cons = k;
  }

let make_t el_list nm tn =
  { masses = el_list; num_mass = nm; trial_num = tn }

let get_masses state = state.masses

let create_state lst mass trial =
  { masses = lst; num_mass = mass; trial_num = trial }

let get_x_pos = Twodspring.get_posx
let get_y_pos = Twodspring.get_posy
let get_x_vel = Twodspring.get_velx
let get_y_vel = Twodspring.get_vely
let zero_state = Twodspring.make_state 0. 0. 0. 0.

let zero_elem_2d =
  {
    motion_vec = zero_state;
    resting_length = 0.;
    mass = 0.;
    spring_cons = 0.;
  }

let scale (v : vector) (s : float) : vector =
  { x = v.x *. s; y = v.y *. s }

let add (v : vector) (u : vector) : vector =
  { x = v.x +. u.x; y = v.y +. u.y }

let magnitude (v : vector) = Float.sqrt ((v.x *. v.x) +. (v.y *. v.y))

let increment (v : vector) (change : vector) dt =
  add v (scale change dt)

let unit_direction (v : vector) : vector = scale v (1.0 /. magnitude v)

let calc_2d_accel
    (prev : element_2d)
    (cur : element_2d)
    (after : element_2d)
    no_after
    no_before
    g =
  add { x = 0.; y = g }
    (add
       (scale
          (unit_direction
             (add
                (if no_before then { x = 750.; y = 475. }
                else get_pos prev.motion_vec)
                (scale (get_pos cur.motion_vec) (-1.))))
          (cur.spring_cons
          *. (magnitude
                (add
                   (get_pos cur.motion_vec)
                   (scale
                      (if no_before then { x = 750.; y = 475. }
                      else get_pos prev.motion_vec)
                      (-1.)))
             -. cur.resting_length)
          /. cur.mass))
       (if no_after then { x = 0.0; y = 0.0 }
       else
         scale
           (unit_direction
              (add
                 (get_pos after.motion_vec)
                 (scale (get_pos cur.motion_vec) (-1.))))
           (after.spring_cons
           *. (magnitude
                 (add
                    (get_pos cur.motion_vec)
                    (scale (get_pos after.motion_vec) (-1.)))
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
      calc_2d_accel prev cur next
        (i < List.length current_config - 1)
        (i = 0) g
      :: !accels
  done;
  List.rev !accels

let update_element (elt : element_2d) (acceleration : vector) dt :
    element_2d =
  {
    motion_vec =
      (let incremented_pos =
         increment (get_pos elt.motion_vec) (get_vel elt.motion_vec) dt
       in
       let incremented_vel =
         increment (get_vel elt.motion_vec) acceleration dt
       in
       make_state incremented_pos.x incremented_pos.y incremented_vel.x
         incremented_vel.y);
    resting_length = elt.resting_length;
    mass = elt.mass;
    spring_cons = elt.spring_cons;
  }

let next_chained_2d_state (current : t) (accelerations : vector list) dt
    =
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

let rec generate_chained_2d_states (cur : t) g dt datapoints acc =
  if cur.trial_num >= datapoints then List.rev acc
  else
    let next_state =
      next_chained_2d_state cur (accel_2d_list cur.masses true g) dt
    in
    generate_chained_2d_states next_state g dt datapoints (cur :: acc)

let rec generate_coordinate_list (cur : element_2d list) acc =
  match cur with
  | [] -> List.rev acc
  | h :: t ->
      generate_coordinate_list t (Twodspring.get_pos h.motion_vec :: acc)

let rec make_ellist_from_coorlst coorlst l m k acc =
  match coorlst with
  | [] -> acc
  | h :: t ->
      make_ellist_from_coorlst t l m k (make_init_elt h l m k :: acc)
