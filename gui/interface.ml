open Graphics

open Unix
(** here's where the graphical interface will be coded (MS2)*)

open Game
open Sys

type box = {
  x : int;
  y : int;
  w : int;
  h : int;
}

type state =
  | StClick
  | StReady
  | StDraw
  | StReset

let current_state = ref StClick

let draw_box box =
  Graphics.moveto box.x box.y;
  Graphics.lineto (box.x + box.w) box.y;
  Graphics.lineto (box.x + box.w) (box.y + box.h);
  Graphics.lineto box.x (box.y + box.h);
  Graphics.lineto box.x box.y

let draw_start_button () =
  let button = { x = 1360; y = 880; w = 140; h = 70 } in
  draw_box button;
  Graphics.moveto 1380 910;
  Graphics.draw_string "START!"

let draw_reset_button () =
  let button = { x = 0; y = 880; w = 140; h = 70 } in
  draw_box button;
  Graphics.moveto 20 910;
  Graphics.draw_string "RESET!"

let get_mass =
  let () = print_string "enter mass: " in
  read_float ()

let get_dt =
  let () = print_string "enter dt from 0 < dt < 0.01: " in
  read_float ()

let get_k =
  let () =
    print_string "enter spring constant 0 < spring constant < 20: "
  in
  read_float ()

let get_l =
  let () = print_string "enter length from 0.5 < length < 160: " in
  read_float ()

let start_velx =
  let () = print_string "enter starting velocityx: " in
  read_float ()

let start_vely =
  let () = print_string "enter starting velocityy: " in
  read_float ()

let data_points =
  let () = print_string "enter number of data points: " in
  read_int ()

let version =
  let () = print_string "enter which version to visualize: " in
  read_int ()

let draw_cords () =
  Graphics.moveto 0 475;
  Graphics.lineto 1500 475;
  Graphics.moveto 4 525;
  Graphics.draw_string "x-axis";
  Graphics.moveto 750 0;
  Graphics.lineto 750 950;
  Graphics.moveto 700 925;
  Graphics.draw_string "y-axis"

let draw_1d posy = draw_circle 750 (posy + 475) 10

let drawpair coor =
  print_string "drawing";
  match coor with
  | x, y -> Graphics.draw_circle x y 10

let linecoor firstc secondc =
  match firstc with
  | x, y -> (
      moveto x y;
      match secondc with
      | xt, yt -> lineto xt yt)

let rec drawchain coorlistBK =
  match coorlistBK with
  | [] -> ()
  | [ h ] ->
      drawpair h;
      linecoor h (750, 475)
  | c :: h :: t ->
      drawpair c;
      linecoor c h;
      drawchain (h :: t)

let rec draw_circle_chain prevx prevy points =
  match points with
  | [] -> failwith "no points"
  | [ (x, y) ] ->
      Graphics.moveto prevx prevy;
      Graphics.lineto x y;
      Graphics.draw_circle x y 10
  | (x, y) :: t ->
      Graphics.moveto prevx prevy;
      Graphics.lineto x y;
      Graphics.draw_circle x y 10;
      draw_circle_chain x y t

let create_1d_arr m dt k l pos vel data =
  let init = Singlespring.create_state pos vel 0 in
  let completed_lst =
    List.rev (Singlespring.start init dt m k l (-9.81) data [])
  in
  Array.of_list completed_lst

let draw_2d posx posy = draw_circle (posx + 750) (posy + 475) 10

let create_2d_arr m dt k l posx posy velx vely data =
  let init = Twodspring.make_state posx posy velx vely in
  let completed_lst =
    Twodspring.generate_states init k m l (-9.81) dt 0 data []
  in
  Array.of_list completed_lst

let rec conversion intlst acc =
  match intlst with
  | [] -> List.rev acc
  | h :: t -> (
      match h with
      | x, y -> conversion t ((float_of_int x, float_of_int y) :: acc))

let rec conversion2 (vectorlst : Twodspring.vector list) acc =
  match vectorlst with
  | [] -> List.rev acc
  | { x; y } :: t ->
      conversion2 t ((int_of_float x, int_of_float y) :: acc)

let create_2d_chain_arr m dt k l data poslist =
  let initlist =
    Multi2dspring.make_ellist_from_coorlst poslist l m k []
  in
  let init_t = Multi2dspring.make_t initlist (List.length initlist) 0 in
  let completedlst =
    Multi2dspring.generate_chained_2d_states init_t (-9.81) dt data []
  in
  Array.of_list completedlst

let create_2d_chain
    (initial_cons : Multi2dspring.element_2d list)
    g
    dt
    datapoints
    [] =
  let completed_lst =
    let initial_state =
      Multi2dspring.make_t initial_cons (List.length initial_cons) 0
    in
    Multi2dspring.generate_chained_2d_states initial_state g dt
      datapoints []
  in
  Array.of_list completed_lst

let draw_initialize =
  let m = get_mass in
  let dt = get_dt in
  let k = get_k in
  let l = get_l in
  let velx = start_velx in
  let vely = start_vely in
  let data = data_points in
  let vers = version in
  Graphics.open_graph " 1500x950";

  let rec run_drawing m dt k l velx vely data vers =
    let points = ref [] in
    while !current_state = StClick do
      if Graphics.button_down () = true then current_state := StReady
      else draw_cords ()
    done;
    let posx = fst (Graphics.mouse_pos ()) in
    let posy = snd (Graphics.mouse_pos ()) in
    points := (posx, posy) :: !points;
    Unix.sleepf 0.1;
    while !current_state = StReady do
      if
        Graphics.button_down () = true
        && fst (Graphics.mouse_pos ()) >= 1360
        && snd (Graphics.mouse_pos ()) >= 880
      then current_state := StDraw
      else if
        Graphics.button_down () = true
        && (fst (Graphics.mouse_pos ()) < 1360
           || snd (Graphics.mouse_pos ()) < 880)
      then (
        let posx = fst (Graphics.mouse_pos ()) in
        let posy = snd (Graphics.mouse_pos ()) in
        points := (posx, posy) :: !points;
        clear_graph ();
        draw_cords ();
        draw_circle_chain 750 475 (List.rev !points);
        draw_start_button ();
        Unix.sleepf 0.1)
      else draw_cords ();
      draw_circle_chain 750 475 (List.rev !points);
      draw_start_button ()
    done;
    let posx = float_of_int (fst (List.nth !points 0)) -. 750. in
    let posy = float_of_int (snd (List.nth !points 0)) -. 475. in
    let completed_1d = create_1d_arr m dt k l posy velx data in
    let length = Array.length completed_1d in
    let completed_2d =
      create_2d_arr m dt k l posx posy velx vely data
    in
    let chainarr =
      create_2d_chain_arr m dt k l data (conversion !points [])
    in
    let length_2d = Array.length completed_2d in
    let length_chain = Array.length chainarr in
    let pos = ref 0 in
    let last = ref (Sys.time ()) in

    while !current_state = StDraw do
      if
        Graphics.button_down () = true
        && fst (Graphics.mouse_pos ()) <= 140
        && snd (Graphics.mouse_pos ()) >= 880
      then current_state := StReset
      else (* print_int length; *)
        draw_cords ();
      draw_reset_button ();
      let cur = Sys.time () in
      if cur -. !last > 0.01 then begin
        clear_graph ();
        if vers = 1 then
          if !pos < length then
            draw_1d
              (int_of_float (Singlespring.get_pos completed_1d.(!pos)));
        (if vers = 2 then
           print_string (string_of_int (List.length !points));
         if !pos < length_chain then
           let cur_t = chainarr.(!pos) in
           let cur_elt_lst = Multi2dspring.get_masses cur_t in
           let positionlst =
             Multi2dspring.generate_coordinate_list cur_elt_lst []
           in

           drawchain (List.rev (conversion2 positionlst [])));
        (if vers = 3 then
         if !pos < length_2d then
           let xcord = Twodspring.get_posx completed_2d.(!pos) in
           let ycord = Twodspring.get_posy completed_2d.(!pos) in

           draw_2d (int_of_float xcord) (int_of_float ycord));
        pos := !pos + 30;
        last := cur
      end
    done;
    Graphics.clear_graph ();
    Unix.sleepf 0.1;
    current_state := StClick;
    run_drawing m dt k l velx vely data vers
  in
  run_drawing m dt k l velx vely data vers

(* 1. In the terminal type "opam install csv" to install csv directory
   2. Download zip and go to src -> results and note that all the csv
   files are empty. 3. Go back to src directory 4. Open utop, type
   #require "csv" 5. Type in # use "core.ml" 6. Then type in demo1 0;;
   7. Type in demo2 0;; 8. Type in demo3 0;; 6. Go to results once more
   to verify the changes have been made :) *)
