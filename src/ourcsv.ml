let rec single_spring_state_list_to_embed_string state_lst =
  " "
  ^
  match state_lst with
  | [] -> ""
  | h :: t ->
      ("\"" ^ string_of_float (Singlespring.get_pos h) ^ "\"\n")
      ^ single_spring_state_list_to_embed_string t

let single_spring_state_list_to_escv state_lst =
  let ecsv =
    Csv.input_all
      (Csv.of_string
         (single_spring_state_list_to_embed_string state_lst))
  in
  let fname = Filename.concat "results" "resultsinglespring.csv" in
  Csv.save fname ecsv

let demo1 =
  let initial_state = Singlespring.create_state 5. 0. 0 in
  single_spring_state_list_to_escv
    (List.rev
       (Singlespring.start initial_state 0.001 1. 4. 1. 9.8 10000 []))

let rec mass_lst_to_string_line (lst : Multispring.element list) =
  match lst with
  | [] -> ""
  | [ x ] -> string_of_float x.position ^ "\"\n"
  | { position; _ } :: t ->
      string_of_float position ^ "\",\"" ^ mass_lst_to_string_line t

let rec verticle_chain_state_lst_to_embed_str lst =
  " "
  ^
  match lst with
  | [] -> ""
  | state :: t ->
      "\""
      ^ mass_lst_to_string_line (Multispring.get_masses state)
      ^ verticle_chain_state_lst_to_embed_str t

let verticle_state_list_to_escv state_lst =
  let ecsv =
    Csv.input_all
      (Csv.of_string (verticle_chain_state_lst_to_embed_str state_lst))
  in
  let fname = Filename.concat "results" "resultmultispring.csv" in
  Csv.save fname ecsv

let demo2 =
  let element1 =
    {
      Multispring.mass = 1.;
      spring_constant = 4.;
      resting_length = 1.;
      position = 5.;
      velocity = 0.;
      id = 0;
    }
  in
  let element2 = { element1 with position = 10. } in
  let initial_state =
    Multispring.create_state [ element1; element2 ] 9.8 0 2
  in
  verticle_state_list_to_escv
    (List.rev
       (Multispring.start_chained_spring initial_state 0.01 1000 []))

let rec state_2d_list_to_embed_string lst =
  " "
  ^
  match lst with
  | [] -> ""
  | h :: t ->
      "\""
      ^ string_of_float (Twodspring.get_pos h).x
      ^ "\",\""
      ^ (string_of_float (Twodspring.get_pos h).y ^ "\"\n")
      ^ state_2d_list_to_embed_string t

let state_list_to_escv state_lst =
  let ecsv =
    Csv.input_all
      (Csv.of_string (state_2d_list_to_embed_string state_lst))
  in
  let fname = Filename.concat "results" "result2dspring.csv" in
  Csv.save fname ecsv

let demo3 =
  let initial_2d_state = Twodspring.make_state 1. 5. 0. 0. in

  state_list_to_escv
    (Twodspring.generate_states initial_2d_state 4. 1. 1. 9.8 0.001 0
       10000 [])

let rec twod_mass_lst_to_string_line
    (lst : Multi2dspring.element_2d list) =
  match lst with
  | [] -> ""
  | [ x ] ->
      string_of_float (Twodspring.get_posx x.motion_vec)
      ^ "\",\""
      ^ string_of_float (Twodspring.get_posy x.motion_vec)
      ^ "\"\n"
  | { motion_vec; _ } :: t ->
      string_of_float (Twodspring.get_posx motion_vec)
      ^ "\",\""
      ^ string_of_float (Twodspring.get_posy motion_vec)
      ^ "\",\""
      ^ twod_mass_lst_to_string_line t

let rec chained_2d_list_to_embed_string lst =
  " "
  ^
  match lst with
  | [] -> ""
  | state :: t ->
      "\""
      ^ twod_mass_lst_to_string_line (Multi2dspring.get_masses state)
      ^ chained_2d_list_to_embed_string t

let chained_2d_list_to_escv state_lst =
  let ecsv =
    Csv.input_all
      (Csv.of_string (chained_2d_list_to_embed_string state_lst))
  in
  let fname = Filename.concat "results" "resultmulti2dspring.csv" in
  Csv.save fname ecsv

let first_mass : Multi2dspring.element_2d =
  let initial_state = Twodspring.make_state 1. 5. 0. 0. in
  {
    motion_vec = initial_state;
    resting_length = 5.0;
    mass = 1.0;
    spring_cons = 4.0;
  }

let second_mass : Multi2dspring.element_2d =
  let initial_state = Twodspring.make_state (-1.) 10. 0. 0. in
  {
    motion_vec = initial_state;
    resting_length = 5.0;
    mass = 1.0;
    spring_cons = 4.0;
  }

let initial_2d_chain_state =
  Multi2dspring.create_state [ first_mass; second_mass ] 2 0

let demo4 =
  chained_2d_list_to_escv
    (Multi2dspring.generate_chained_2d_states initial_2d_chain_state 9.8
       0.1 1000 [])
