open Graphics
open Game
open OUnit2

let pp_string s = "\"" ^ s ^ "\""

let oned_to_string (state : Singlespring.t) =
  "{ " ^ "position: "
  ^ string_of_float (Singlespring.get_pos state)
  ^ ", velocity: "
  ^ string_of_float (Singlespring.get_vel state)
  ^ ", index: "
  ^ string_of_int (Singlespring.get_index state)
  ^ "}"

let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

let sslst lst =
  List.fold_right (fun ele acc -> oned_to_string ele :: acc) [] lst
(* let twod_to_string (state: Twodspring.t) = raise (failwith "") *)

let create_state_test
    (name : string)
    (pos : float)
    (vel : float)
    index
    expected_output : test =
  name >:: fun _ ->
  (* the [printer] tells OUnit how to convert the output to a string *)
  assert_equal expected_output
    (oned_to_string (Singlespring.create_state pos vel index))
    ~printer:pp_string

let get_pos_test name singlespring expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output
    (Singlespring.get_pos singlespring)
    ~printer:string_of_float

let get_vel_test name singlespring expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output
    (Singlespring.get_vel singlespring)
    ~printer:string_of_float

let accel_test name singlespring mass k length expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output
    (Singlespring.cur_accel
       (Singlespring.get_pos singlespring)
       mass k length 9.81)
    ~printer:string_of_float

let next_state_test name singlespring dt mass k l expected_output : test
    =
  name >:: fun _ ->
  assert_equal expected_output
    (oned_to_string
       (Singlespring.next_state singlespring dt mass k l 9.81))
    ~printer:pp_string

let start_test name init dt m k l expected_output =
  name >:: fun _ ->
  assert_equal expected_output
    (pp_list oned_to_string
       (Singlespring.start init dt m k l 9.81 100 []))
    ~printer:pp_string

let zerozerozero = Singlespring.create_state 0. 0. 0
let zerotenzero = Singlespring.create_state 0. 10. 0
let tenzerozero = Singlespring.create_state 10. 0. 0
let tententen = Singlespring.create_state 10. 10. 10
let onehundredthree = Singlespring.create_state 100. 100. 0

let single_spring_tests =
  [
    create_state_test "x pos 0, 0 velocity, index 0" 0. 0. 0
      "{ position: 0., velocity: 0., index: 0}";
    create_state_test "x pos is 5, velocity is 2, index is 0" 5. 2. 0
      "{ position: 5., velocity: 2., index: 0}";
    create_state_test "x pos is 5, velocity is 2, index is 0" 5. 2. 0
      "{ position: 5., velocity: 2., index: 0}";
    create_state_test "x pos is 10, velocity is 10, index is 5" 5. 2. 5
      "{ position: 5., velocity: 2., index: 5}";
    create_state_test "x pos is 0, velocity is 20, index is 0" 0. 20. 0
      "{ position: 0., velocity: 20., index: 0}";
    get_pos_test "pos is 0" zerozerozero 0.;
    get_pos_test "pos is 0" zerotenzero 0.;
    get_pos_test "pos is ten" tenzerozero 10.;
    get_pos_test "pos is ten" tententen 10.;
    get_pos_test "pos is 100" onehundredthree 100.;
    get_vel_test "vel is 0" zerozerozero 0.;
    get_vel_test "vel is ten" zerotenzero 10.;
    get_vel_test "vel is 0" tenzerozero 0.;
    get_vel_test "vel is ten" tententen 10.;
    get_vel_test "vel is 100" onehundredthree 100.;
    accel_test "mass 5, springconstant 5, length 5" zerozerozero 5. 5.
      5. 14.81;
    accel_test "mass 1, springconstant 1, length 1" zerozerozero 1. 1.
      1. 10.81;
    accel_test "mass 1, springconstant 1, length 1" zerozerozero 1. 1.
      1. 10.81;
    accel_test "only pos matters" zerotenzero 1. 1. 1. 10.81;
    accel_test "mass is 0.1, springconstant 5, length 0.5" zerozerozero
      0.1 5. 0.5 34.81;
    accel_test "mass is 0, springconstant 5, length 0.5" zerozerozero
      0.1 5. 0.5 34.81;
    accel_test "mass is 10, springconstant 5, length 10" zerozerozero
      10. 5. 10. 14.81;
    next_state_test "mass is 5, k is 5, l is 5" zerozerozero 0.05 5. 5.
      5. "{ position: 0., velocity: 0.7405, index: 1}";
    next_state_test "mass is 5, k is 5, l is 5" tenzerozero 0.05 5. 5.
      5. "{ position: 10., velocity: 0.2405, index: 1}";
    next_state_test "mass is 5, k is 5, l is 5" tententen 0.05 5. 5. 5.
      "{ position: 10.5, velocity: 10.2405, index: 11}";
    next_state_test "mass is 5, k is 5, l is 5" onehundredthree 0.05 5.
      5. 5. "{ position: 105., velocity: 95.7405, index: 1}";
    next_state_test "mass is 1, k is 1, l is 1 " zerotenzero 0.05 1. 1.
      1. "{ position: 0.5, velocity: 10.5405, index: 1}";
    start_test "zerozerozero" zerozerozero 0.05 5. 5. 1.
      "[{ position: 7.38476035483, velocity: -11.7587045092, index: \
       100}; { position: 7.97977115241, velocity: -11.9002159516, \
       index: 99}; { position: 8.58035605983, velocity: \
       -12.0116981486, index: 98}; { position: 9.18500345862, \
       velocity: -12.0929479756, index: 97}; { position: \
       9.79219536897, velocity: -12.1438382072, index: 96}; { \
       position: 10.4004112512, velocity: -12.1643176446, index: 95}; \
       { position: 11.0081318039, velocity: -12.1544110544, index: \
       94}; { position: 11.6138427498, velocity: -12.1142189169, \
       index: 93}; { position: 12.2160385991, velocity: -12.043916987, \
       index: 92}; { position: 12.8132263825, velocity: \
       -11.9437556679, index: 91}; { position: 13.4039293426, \
       velocity: -11.8140592007, index: 90}; { position: \
       13.9866905761, velocity: -11.6552246719, index: 89}; { \
       position: 14.5600766182, velocity: -11.467720841, index: 88}; { \
       position: 15.1226809579, velocity: -11.2520867931, index: 87}; \
       { position: 15.6731274788, velocity: -11.0089304192, index: \
       86}; { position: 16.2100738152, velocity: -10.7389267284, \
       index: 85}; { position: 16.7322146151, velocity: \
       -10.4428159977, index: 84}; { position: 17.2382847032, \
       velocity: -10.1214017625, index: 83}; { position: 17.727062136, \
       velocity: -9.7755486557, index: 82}; { position: 18.197371141, \
       velocity: -9.40618009865, index: 81}; { position: \
       18.6480849336, velocity: -9.01427585197, index: 80}; { \
       position: 19.0781284051, velocity: -8.60086943171, index: 79}; \
       { position: 19.486480675, velocity: -8.16704539796, index: 78}; \
       { position: 19.8721775012, velocity: -7.7139365229, index: 77}; \
       { position: 20.2343135435, velocity: -7.24272084573, index: \
       76}; { position: 20.5720444746, velocity: -6.754618622, index: \
       75}; { position: 20.8845889333, velocity: -6.25088917533, \
       index: 74}; { position: 21.1712303163, velocity: \
       -5.73282765952, index: 73}; { position: 21.4313184033, \
       velocity: -5.20176173935, index: 72}; { position: \
       21.6642708132, velocity: -4.65904819869, index: 71}; { \
       position: 21.8695742874, velocity: -4.10606948432, index: 70}; \
       { position: 22.0467857972, velocity: -3.54423019446, index: \
       69}; { position: 22.1955334732, velocity: -2.9749535208, index: \
       68}; { position: 22.3155173558, velocity: -2.39967765301, \
       index: 67}; { position: 22.4065099636, velocity: \
       -1.81985215483, index: 66}; { position: 22.4683566796, \
       velocity: -1.23693432085, index: 65}; { position: \
       22.5009759558, velocity: -0.65238552306, index: 64}; { \
       position: 22.5043593336, velocity: -0.06766755638, index: 63}; \
       { position: 22.4785712832, velocity: 0.515761007781, index: \
       62}; { position: 22.4237488607, velocity: 1.09644845081, index: \
       61}; { position: 22.3401011852, velocity: 1.67295351007, index: \
       60}; { position: 22.2279087378, velocity: 2.24384894696, index: \
       59}; { position: 22.0875224843, velocity: 2.80772507118, index: \
       58}; { position: 21.9193628236, velocity: 3.36319321236, index: \
       57}; { position: 21.7239183671, velocity: 3.90888913071, index: \
       56}; { position: 21.5017445492, velocity: 4.44347635817, index: \
       55}; { position: 21.2534620761, velocity: 4.96564946198, index: \
       54}; { position: 20.979755215, velocity: 5.47413722273, index: \
       53}; { position: 20.681369929, velocity: 5.96770571918, index: \
       52}; { position: 20.3591118634, velocity: 6.44516131235, index: \
       51}; { position: 20.0138441873, velocity: 6.90535352171, index: \
       50}; { position: 19.646485298, velocity: 7.34717778661, index: \
       49}; { position: 19.2580063927, velocity: 7.76957810624, index: \
       48}; { position: 18.8494289151, velocity: 8.171549552, index: \
       47}; { position: 18.4218218828, velocity: 8.55214064613, index: \
       46}; { position: 17.9762991027, velocity: 8.91045560127, index: \
       45}; { position: 17.5140162819, velocity: 9.24565641537, index: \
       44}; { position: 17.0361680411, velocity: 9.55696481742, index: \
       43}; { position: 16.5439848381, velocity: 9.84366405932, index: \
       42}; { position: 16.0387298106, velocity: 10.1051005499, index: \
       41}; { position: 15.5216955442, velocity: 10.3406853271, index: \
       40}; { position: 14.9942007759, velocity: 10.5498953659, index: \
       39}; { position: 14.4575870401, velocity: 10.7322747179, index: \
       38}; { position: 13.913215266, velocity: 10.8874354812, index: \
       37}; { position: 13.3624623361, velocity: 11.015058598, index: \
       36}; { position: 12.8067176122, velocity: 11.1148944786, index: \
       35}; { position: 12.2473794396, velocity: 11.1867634506, index: \
       34}; { position: 11.685851638, velocity: 11.2305560325, index: \
       33}; { position: 11.1235399864, velocity: 11.2462330318, index: \
       32}; { position: 10.5618487131, velocity: 11.2338254674, index: \
       31}; { position: 10.0021769972, velocity: 11.1934343173, index: \
       30}; { position: 9.44591549259, velocity: 11.1252300919, index: \
       29}; { position: 8.8944428808, velocity: 11.029452236, index: \
       28}; { position: 8.34912246284, velocity: 10.9064083591, index: \
       27}; { position: 7.81129879789, velocity: 10.756473299, index: \
       26}; { position: 7.28229439695, velocity: 10.5800880188, index: \
       25}; { position: 6.76340647981, velocity: 10.3777583428, index: \
       24}; { position: 6.25590380316, velocity: 10.150053533, index: \
       23}; { position: 5.76102356759, velocity: 9.89760471137, index: \
       22}; { position: 5.27996841099, velocity: 9.62110313192, index: \
       21}; { position: 4.81390349566, velocity: 9.32129830671, index: \
       20}; { position: 4.36395369608, velocity: 8.99899599151, index: \
       19}; { position: 3.93120089427, velocity: 8.65505603622, index: \
       18}; { position: 3.51668138899, velocity: 8.29039010567, index: \
       17}; { position: 3.12138342514, velocity: 7.90595927693, index: \
       16}; { position: 2.74624484917, velocity: 7.50277151939, index: \
       15}; { position: 2.39215089596, velocity: 7.08187906419, index: \
       14}; { position: 2.05993211247, velocity: 6.64437566981, index: \
       13}; { position: 1.75036242292, velocity: 6.19139379096, index: \
       12}; { position: 1.46415734003, velocity: 5.72410165796, index: \
       11}; { position: 1.20197232631, velocity: 5.24370027427, index: \
       10}; { position: 0.964401309325, velocity: 4.75142033974, \
       index: 9}; { position: 0.751975353953, velocity: 4.24851910744, \
       index: 8}; { position: 0.565161494844, velocity: 3.73627718218, \
       index: 7}; { position: 0.404361731406, velocity: 3.21599526875, \
       index: 6}; { position: 0.2699121875, velocity: 2.68899087813, \
       index: 5}; { position: 0.1620824375, velocity: 2.156595, index: \
       4}; { position: 0.081075, velocity: 1.62014875, index: 3}; { \
       position: 0.027025, velocity: 1.081, index: 2}; { position: 0., \
       velocity: 0.5405, index: 1}; { position: 0., velocity: 0., \
       index: 0}]";
    start_test "onehundred" onehundredthree 0.05 5. 5. 1.
      "[{ position: -69.7055714121, velocity: 128.70331357, index: \
       100}; { position: -75.9239023347, velocity: 124.366618453, \
       index: 99}; { position: -81.9104321769, velocity: \
       119.730596844, index: 98}; { position: -87.6508099942, \
       velocity: 114.807556345, index: 97}; { position: \
       -93.1313344752, velocity: 109.610489621, index: 96}; { \
       position: -98.33898649, velocity: 104.153040296, index: 95}; { \
       position: -103.261459855, velocity: 98.4494673036, index: 94}; \
       { position: -107.887190245, velocity: 92.5146077913, index: \
       93}; { position: -112.205382179, velocity: 86.3638386824, \
       index: 92}; { position: -116.206034028, velocity: 80.013036981, \
       index: 91}; { position: -119.879960975, velocity: \
       73.4785389323, index: 90}; { position: -123.218815881, \
       velocity: 66.7770981382, index: 89}; { position: \
       -126.215108018, velocity: 59.9258427373, index: 88}; { \
       position: -128.862219606, velocity: 52.942231757, index: 87}; { \
       position: -131.154420144, velocity: 45.8440107498, index: 86}; \
       { position: -133.086878485, velocity: 38.6491668255, index: \
       85}; { position: -134.655672645, velocity: 31.3758831933, \
       index: 84}; { position: -135.857797311, velocity: \
       24.0424933278, index: 83}; { position: -136.691169055, \
       velocity: 16.667434875, index: 82}; { position: -137.154629225, \
       velocity: 9.26920341374, index: 81}; { position: \
       -137.247944535, velocity: 1.866306187, index: 80}; { position: \
       -136.971805331, velocity: -5.52278407954, index: 79}; { \
       position: -136.327821573, velocity: -12.8796751582, index: 78}; \
       { position: -135.318516524, velocity: -20.1861009844, index: \
       77}; { position: -133.947318179, velocity: -27.4239668933, \
       index: 76}; { position: -132.218548463, velocity: \
       -34.5753943165, index: 75}; { position: -130.137410222, \
       velocity: -41.6227648276, index: 74}; { position: \
       -127.70997205, velocity: -48.5487634301, index: 73}; { \
       position: -124.943151001, velocity: -55.3364209801, index: 72}; \
       { position: -121.844693219, velocity: -61.9691556411, index: \
       71}; { position: -118.423152556, velocity: -68.4308132689, \
       index: 70}; { position: -114.687867224, velocity: \
       -74.7057066301, index: 69}; { position: -110.648934556, \
       velocity: -80.7786533579, index: 68}; { position: \
       -106.317183929, velocity: -86.6350125544, index: 67}; { \
       position: -101.704147931, velocity: -92.2607199509, index: 66}; \
       { position: -96.8220318539, velocity: -97.6423215436, index: \
       65}; { position: -91.6836815728, velocity: -102.767005622, \
       index: 64}; { position: -86.3025499169, velocity: \
       -107.622633118, index: 63}; { position: -80.692661607, \
       velocity: -112.197766198, index: 62}; { position: \
       -74.8685768549, velocity: -116.481695041, index: 61}; { \
       position: -68.8453537186, velocity: -120.464462727, index: 60}; \
       { position: -62.638509309, velocity: -124.136888193, index: \
       59}; { position: -56.2639799495, velocity: -127.49058719, \
       index: 58}; { position: -49.738080389, velocity: -130.51799121, \
       index: 57}; { position: -43.0774621731, velocity: \
       -133.212364318, index: 56}; { position: -36.299071279, \
       velocity: -135.567817882, index: 55}; { position: \
       -29.4201051221, velocity: -137.579323138, index: 54}; { \
       position: -22.4579690425, velocity: -139.24272159, index: 53}; \
       { position: -15.4302323821, velocity: -140.554733209, index: \
       52}; { position: -8.35458426095, velocity: -141.512962422, \
       index: 51}; { position: -1.2487891669, velocity: \
       -142.115901881, index: 50}; { position: 5.8693575333, velocity: \
       -142.362934004, index: 49}; { position: 12.9820740484, \
       velocity: -142.254330302, index: 48}; { position: \
       20.0716364723, velocity: -141.791248478, index: 47}; { \
       position: 27.1204228391, velocity: -140.975727336, index: 46}; \
       { position: 34.1109568139, velocity: -139.810679495, index: \
       45}; { position: 41.0259509114, velocity: -138.29988195, index: \
       44}; { position: 47.848349136, velocity: -136.447964493, index: \
       43}; { position: 54.5613689383, velocity: -134.260396046, \
       index: 42}; { position: 61.1485423847, velocity: \
       -131.743468927, index: 41}; { position: 67.5937564399, \
       velocity: -128.904281105, index: 40}; { position: \
       73.8812922645, velocity: -125.750716492, index: 39}; { \
       position: 79.9958634305, velocity: -122.29142332, index: 38}; { \
       position: 85.9226529641, velocity: -118.535790672, index: 37}; \
       { position: 91.6473491249, velocity: -114.493923216, index: \
       36}; { position: 97.1561798361, velocity: -110.176614224, \
       index: 35}; { position: 102.435945683, velocity: -105.59531694, \
       index: 34}; { position: 107.474051402, velocity: -100.76211437, \
       index: 33}; { position: 112.258535781, velocity: \
       -95.6896875807, index: 32}; { position: 116.77809991, velocity: \
       -90.3912825852, index: 31}; { position: 121.022133705, \
       velocity: -84.8806758999, index: 30}; { position: \
       124.980740648, velocity: -79.1721388675, index: 29}; { \
       position: 128.64476069, velocity: -73.280400833, index: 28}; { \
       position: 132.005791253, velocity: -67.2206112704, index: 27}; \
       { position: 135.056206301, velocity: -61.0083009553, index: \
       26}; { position: 137.789173415, velocity: -54.6593422845, \
       index: 25}; { position: 140.198668858, velocity: \
       -48.1899088417, index: 24}; { position: 142.279490573, \
       velocity: -41.616434313, index: 23}; { position: 144.027269116, \
       velocity: -34.9555708572, index: 22}; { position: \
       145.438476468, velocity: -28.2241470338, index: 21}; { \
       position: 146.510432738, velocity: -21.4391253969, index: 20}; \
       { position: 147.241310731, velocity: -14.6175598604, index: \
       19}; { position: 147.630138378, velocity: -7.77655294152, \
       index: 18}; { position: 147.676799027, velocity: \
       -0.933212990167, index: 17}; { position: 147.382029603, \
       velocity: 5.89538848997, index: 16}; { position: 146.747416637, \
       velocity: 12.6922593218, index: 15}; { position: 145.775390195, \
       velocity: 19.4405288315, index: 14}; { position: 144.469215714, \
       velocity: 26.1234896173, index: 13}; { position: 142.832983774, \
       velocity: 32.7246388059, index: 12}; { position: 140.871597839, \
       velocity: 39.2277186979, index: 11}; { position: 138.590760004, \
       velocity: 45.6167566981, index: 10}; { position: 135.996954782, \
       velocity: 51.8761044372, index: 9}; { position: 133.097430983, \
       velocity: 57.9904759863, index: 8}; { position: 129.900181729, \
       velocity: 63.9449850728, index: 7}; { position: 126.413922669, \
       velocity: 69.7251812062, index: 6}; { position: 122.648068437, \
       velocity: 75.3170846281, index: 5}; { position: 118.612707437, \
       velocity: 80.70722, index: 4}; { position: 114.318575, \
       velocity: 85.88264875, index: 3}; { position: 109.777025, \
       velocity: 90.831, index: 2}; { position: 105., velocity: \
       95.5405, index: 1}; { position: 100., velocity: 100., index: \
       0}]";
    start_test "onehundredstart" onehundredthree 0.05 5. 5. 1.
      "[{ position: -69.7055714121, velocity: 128.70331357, index: \
       100}; { position: -75.9239023347, velocity: 124.366618453, \
       index: 99}; { position: -81.9104321769, velocity: \
       119.730596844, index: 98}; { position: -87.6508099942, \
       velocity: 114.807556345, index: 97}; { position: \
       -93.1313344752, velocity: 109.610489621, index: 96}; { \
       position: -98.33898649, velocity: 104.153040296, index: 95}; { \
       position: -103.261459855, velocity: 98.4494673036, index: 94}; \
       { position: -107.887190245, velocity: 92.5146077913, index: \
       93}; { position: -112.205382179, velocity: 86.3638386824, \
       index: 92}; { position: -116.206034028, velocity: 80.013036981, \
       index: 91}; { position: -119.879960975, velocity: \
       73.4785389323, index: 90}; { position: -123.218815881, \
       velocity: 66.7770981382, index: 89}; { position: \
       -126.215108018, velocity: 59.9258427373, index: 88}; { \
       position: -128.862219606, velocity: 52.942231757, index: 87}; { \
       position: -131.154420144, velocity: 45.8440107498, index: 86}; \
       { position: -133.086878485, velocity: 38.6491668255, index: \
       85}; { position: -134.655672645, velocity: 31.3758831933, \
       index: 84}; { position: -135.857797311, velocity: \
       24.0424933278, index: 83}; { position: -136.691169055, \
       velocity: 16.667434875, index: 82}; { position: -137.154629225, \
       velocity: 9.26920341374, index: 81}; { position: \
       -137.247944535, velocity: 1.866306187, index: 80}; { position: \
       -136.971805331, velocity: -5.52278407954, index: 79}; { \
       position: -136.327821573, velocity: -12.8796751582, index: 78}; \
       { position: -135.318516524, velocity: -20.1861009844, index: \
       77}; { position: -133.947318179, velocity: -27.4239668933, \
       index: 76}; { position: -132.218548463, velocity: \
       -34.5753943165, index: 75}; { position: -130.137410222, \
       velocity: -41.6227648276, index: 74}; { position: \
       -127.70997205, velocity: -48.5487634301, index: 73}; { \
       position: -124.943151001, velocity: -55.3364209801, index: 72}; \
       { position: -121.844693219, velocity: -61.9691556411, index: \
       71}; { position: -118.423152556, velocity: -68.4308132689, \
       index: 70}; { position: -114.687867224, velocity: \
       -74.7057066301, index: 69}; { position: -110.648934556, \
       velocity: -80.7786533579, index: 68}; { position: \
       -106.317183929, velocity: -86.6350125544, index: 67}; { \
       position: -101.704147931, velocity: -92.2607199509, index: 66}; \
       { position: -96.8220318539, velocity: -97.6423215436, index: \
       65}; { position: -91.6836815728, velocity: -102.767005622, \
       index: 64}; { position: -86.3025499169, velocity: \
       -107.622633118, index: 63}; { position: -80.692661607, \
       velocity: -112.197766198, index: 62}; { position: \
       -74.8685768549, velocity: -116.481695041, index: 61}; { \
       position: -68.8453537186, velocity: -120.464462727, index: 60}; \
       { position: -62.638509309, velocity: -124.136888193, index: \
       59}; { position: -56.2639799495, velocity: -127.49058719, \
       index: 58}; { position: -49.738080389, velocity: -130.51799121, \
       index: 57}; { position: -43.0774621731, velocity: \
       -133.212364318, index: 56}; { position: -36.299071279, \
       velocity: -135.567817882, index: 55}; { position: \
       -29.4201051221, velocity: -137.579323138, index: 54}; { \
       position: -22.4579690425, velocity: -139.24272159, index: 53}; \
       { position: -15.4302323821, velocity: -140.554733209, index: \
       52}; { position: -8.35458426095, velocity: -141.512962422, \
       index: 51}; { position: -1.2487891669, velocity: \
       -142.115901881, index: 50}; { position: 5.8693575333, velocity: \
       -142.362934004, index: 49}; { position: 12.9820740484, \
       velocity: -142.254330302, index: 48}; { position: \
       20.0716364723, velocity: -141.791248478, index: 47}; { \
       position: 27.1204228391, velocity: -140.975727336, index: 46}; \
       { position: 34.1109568139, velocity: -139.810679495, index: \
       45}; { position: 41.0259509114, velocity: -138.29988195, index: \
       44}; { position: 47.848349136, velocity: -136.447964493, index: \
       43}; { position: 54.5613689383, velocity: -134.260396046, \
       index: 42}; { position: 61.1485423847, velocity: \
       -131.743468927, index: 41}; { position: 67.5937564399, \
       velocity: -128.904281105, index: 40}; { position: \
       73.8812922645, velocity: -125.750716492, index: 39}; { \
       position: 79.9958634305, velocity: -122.29142332, index: 38}; { \
       position: 85.9226529641, velocity: -118.535790672, index: 37}; \
       { position: 91.6473491249, velocity: -114.493923216, index: \
       36}; { position: 97.1561798361, velocity: -110.176614224, \
       index: 35}; { position: 102.435945683, velocity: -105.59531694, \
       index: 34}; { position: 107.474051402, velocity: -100.76211437, \
       index: 33}; { position: 112.258535781, velocity: \
       -95.6896875807, index: 32}; { position: 116.77809991, velocity: \
       -90.3912825852, index: 31}; { position: 121.022133705, \
       velocity: -84.8806758999, index: 30}; { position: \
       124.980740648, velocity: -79.1721388675, index: 29}; { \
       position: 128.64476069, velocity: -73.280400833, index: 28}; { \
       position: 132.005791253, velocity: -67.2206112704, index: 27}; \
       { position: 135.056206301, velocity: -61.0083009553, index: \
       26}; { position: 137.789173415, velocity: -54.6593422845, \
       index: 25}; { position: 140.198668858, velocity: \
       -48.1899088417, index: 24}; { position: 142.279490573, \
       velocity: -41.616434313, index: 23}; { position: 144.027269116, \
       velocity: -34.9555708572, index: 22}; { position: \
       145.438476468, velocity: -28.2241470338, index: 21}; { \
       position: 146.510432738, velocity: -21.4391253969, index: 20}; \
       { position: 147.241310731, velocity: -14.6175598604, index: \
       19}; { position: 147.630138378, velocity: -7.77655294152, \
       index: 18}; { position: 147.676799027, velocity: \
       -0.933212990167, index: 17}; { position: 147.382029603, \
       velocity: 5.89538848997, index: 16}; { position: 146.747416637, \
       velocity: 12.6922593218, index: 15}; { position: 145.775390195, \
       velocity: 19.4405288315, index: 14}; { position: 144.469215714, \
       velocity: 26.1234896173, index: 13}; { position: 142.832983774, \
       velocity: 32.7246388059, index: 12}; { position: 140.871597839, \
       velocity: 39.2277186979, index: 11}; { position: 138.590760004, \
       velocity: 45.6167566981, index: 10}; { position: 135.996954782, \
       velocity: 51.8761044372, index: 9}; { position: 133.097430983, \
       velocity: 57.9904759863, index: 8}; { position: 129.900181729, \
       velocity: 63.9449850728, index: 7}; { position: 126.413922669, \
       velocity: 69.7251812062, index: 6}; { position: 122.648068437, \
       velocity: 75.3170846281, index: 5}; { position: 118.612707437, \
       velocity: 80.70722, index: 4}; { position: 114.318575, \
       velocity: 85.88264875, index: 3}; { position: 109.777025, \
       velocity: 90.831, index: 2}; { position: 105., velocity: \
       95.5405, index: 1}; { position: 100., velocity: 100., index: \
       0}]";
    start_test "onehundredstart" tententen 0.05 1. 1. 1.
      "[{ position: 0.0755403108414, velocity: -3.28479938743, index: \
       100}; { position: 0.266139930387, velocity: -3.81199239091, \
       index: 99}; { position: 0.482558154546, velocity: \
       -4.32836448318, index: 98}; { position: 0.724190901451, \
       velocity: -4.83265493811, index: 97}; { position: \
       0.990372716565, velocity: -5.32363630228, index: 96}; { \
       position: 1.28037858522, velocity: -5.80011737302, index: 95}; \
       { position: 1.59342588914, velocity: -6.26094607856, index: \
       94}; { position: 1.92867650182, velocity: -6.70501225347, \
       index: 93}; { position: 2.28523901695, velocity: \
       -7.13125030262, index: 92}; { position: 2.66217110432, \
       velocity: -7.53864174741, index: 91}; { position: \
       3.05848198672, velocity: -7.92621764807, index: 90}; { \
       position: 3.47313503155, velocity: -8.2930608965, index: 89}; { \
       position: 3.90505045025, velocity: -8.63830837398, index: 88}; \
       { position: 4.3531080987, velocity: -8.96115296905, index: 87}; \
       { position: 4.81615037122, velocity: -9.26084545049, index: \
       86}; { position: 5.2929851808, velocity: -9.53669619145, index: \
       85}; { position: 5.78238901782, velocity: -9.78807674056, \
       index: 84}; { position: 6.28311007965, velocity: \
       -10.0144212366, index: 83}; { position: 6.79387146282, \
       velocity: -10.2152276634, index: 82}; { position: \
       7.31337440997, velocity: -10.3900589429, index: 81}; { \
       position: 7.84030160311, velocity: -10.5385438628, index: 80}; \
       { position: 8.37332049501, velocity: -10.660377838, index: 79}; \
       { position: 8.91108667024, velocity: -10.7553235045, index: \
       78}; { position: 9.45224722739, velocity: -10.8232111431, \
       index: 77}; { position: 9.99544417412, velocity: \
       -10.8639389344, index: 76}; { position: 10.5393178263, \
       velocity: -10.8774730431, index: 75}; { position: \
       11.0825102029, velocity: -10.863847533, index: 74}; { position: \
       11.6236684085, velocity: -10.8231641126, index: 73}; { \
       position: 12.1614479942, velocity: -10.7555917128, index: 72}; \
       { position: 12.6945162891, velocity: -10.6613658984, index: \
       71}; { position: 13.2215556948, velocity: -10.5407881136, \
       index: 70}; { position: 13.7412669331, velocity: -10.394224767, \
       index: 69}; { position: 14.2523722409, velocity: \
       -10.2221061549, index: 68}; { position: 14.7536185024, \
       velocity: -10.0249252298, index: 67}; { position: \
       15.2437803131, velocity: -9.80323621417, index: 66}; { \
       position: 15.7216629664, velocity: -9.55765306585, index: 65}; \
       { position: 16.1861053563, velocity: -9.28884779804, index: \
       64}; { position: 16.6359827892, velocity: -8.99754865858, \
       index: 63}; { position: 17.0702096979, velocity: \
       -8.68453817368, index: 62}; { position: 17.487742251, velocity: \
       -8.35065106114, index: 61}; { position: 17.8875808519, \
       velocity: -7.99677201854, index: 60}; { position: \
       18.2687725215, velocity: -7.62383339247, index: 59}; { \
       position: 18.6304131582, velocity: -7.23281273455, index: 58}; \
       { position: 18.9716496708, velocity: -6.82473025102, index: \
       57}; { position: 19.2916819784, velocity: -6.4006461521, index: \
       56}; { position: 19.5897648738, velocity: -5.96165790841, \
       index: 55}; { position: 19.8652097449, velocity: \
       -5.50889742116, index: 54}; { position: 20.1173861505, \
       velocity: -5.04352811364, index: 53}; { position: \
       20.3457232481, velocity: -4.56674195123, index: 52}; { \
       position: 20.549711068, velocity: -4.07975639783, index: 51}; { \
       position: 20.7289016338, velocity: -3.58381131614, index: 50}; \
       { position: 20.8829099248, velocity: -3.0801658199, index: 49}; \
       { position: 21.0114146791, velocity: -2.57009508594, index: \
       48}; { position: 21.1141590358, velocity: -2.05488713415, \
       index: 47}; { position: 21.190951015, velocity: -1.5358395834, \
       index: 46}; { position: 21.2416638346, velocity: \
       -1.01425639168, index: 45}; { position: 21.266236064, velocity: \
       -0.491444588477, index: 44}; { position: 21.2646716144, \
       velocity: 0.0312889922415, index: 43}; { position: \
       21.2370395658, velocity: 0.552640970534, index: 42}; { \
       position: 21.1834738327, velocity: 1.07131466217, index: 41}; { \
       position: 21.104172668, velocity: 1.58602329557, index: 40}; { \
       position: 20.9993980082, velocity: 2.09549319598, index: 39}; { \
       position: 20.8694746617, velocity: 2.59846692906, index: 38}; { \
       position: 20.7147893419, velocity: 3.09370639616, index: 37}; { \
       position: 20.5357895482, velocity: 3.57999587357, index: 36}; { \
       position: 20.3329822988, velocity: 4.05614498851, index: 35}; { \
       position: 20.1069327176, velocity: 4.52099162439, index: 34}; { \
       position: 19.8582624802, velocity: 4.9734047484, index: 33}; { \
       position: 19.5876481224, velocity: 5.41228715452, index: 32}; { \
       position: 19.2958192167, velocity: 5.83657811535, index: 31}; { \
       position: 18.9835564198, velocity: 6.24525593634, index: 30}; { \
       position: 18.6516893995, velocity: 6.63734040632, index: 29}; { \
       position: 18.3010946426, velocity: 7.01189513845, index: 28}; { \
       position: 17.9326931528, velocity: 7.36802979609, index: 27}; { \
       position: 17.5474480429, velocity: 7.70490219823, index: 26}; { \
       position: 17.1463620279, velocity: 8.02172029963, index: 25}; { \
       position: 16.7304748259, velocity: 8.31774404092, index: 24}; { \
       position: 16.3008604726, velocity: 8.59228706455, index: 23}; { \
       position: 15.858624558, velocity: 8.84471829246, index: 22}; { \
       position: 15.4049013899, velocity: 9.07446336195, index: 21}; { \
       position: 14.9408510941, velocity: 9.28100591666, index: 20}; { \
       position: 14.4676566566, velocity: 9.46388874949, index: 19}; { \
       position: 13.9865209168, velocity: 9.62271479533, index: 18}; { \
       position: 13.4986635183, velocity: 9.75714797124, index: 17}; { \
       position: 13.0053178252, velocity: 9.8669138625, index: 16}; { \
       position: 12.5077278125, velocity: 9.95180025313, index: 15}; { \
       position: 12.0071449375, velocity: 10.0116575, index: 14}; { \
       position: 11.504825, velocity: 10.04639875, index: 13}; { \
       position: 11.002025, velocity: 10.056, index: 12}; { position: \
       10.5, velocity: 10.0405, index: 11}; { position: 10., velocity: \
       10., index: 10}]";
    start_test "onehundred" tenzerozero 0.05 5. 1. 1.
      "[{ position: 75.3854169725, velocity: 14.452245796, index: \
       100}; { position: 74.6505044305, velocity: 14.6982508403, \
       index: 99}; { position: 73.9036650559, velocity: 14.9367874908, \
       index: 98}; { position: 73.1452780423, velocity: 15.1677402712, \
       index: 97}; { position: 72.3757281647, velocity: 15.3909975529, \
       index: 96}; { position: 71.5954055843, velocity: 15.6064516087, \
       index: 95}; { position: 70.804705651, velocity: 15.8139986652, \
       index: 94}; { position: 70.0040287034, velocity: 16.0135389523, \
       index: 93}; { position: 69.1937798658, velocity: 16.2049767509, \
       index: 92}; { position: 68.3743688439, velocity: 16.3882204394, \
       index: 91}; { position: 67.5462097171, velocity: 16.5631825365, \
       index: 90}; { position: 66.7097207299, velocity: 16.7297797438, \
       index: 89}; { position: 65.8653240806, velocity: 16.8879329846, \
       index: 88}; { position: 65.0134457085, velocity: 17.0375674417, \
       index: 87}; { position: 64.1545150789, velocity: 17.1786125925, \
       index: 86}; { position: 63.2889649668, velocity: 17.3110022422, \
       index: 85}; { position: 62.4172312391, velocity: 17.4346745546, \
       index: 84}; { position: 61.539752635, velocity: 17.5495720809, \
       index: 83}; { position: 60.6569705457, velocity: 17.6556417864, \
       index: 82}; { position: 59.769328792, velocity: 17.7528350743, \
       index: 81}; { position: 58.8772734016, velocity: 17.8411078083, \
       index: 80}; { position: 57.981252385, velocity: 17.9204203322, \
       index: 79}; { position: 57.0817155106, velocity: 17.9907374873, \
       index: 78}; { position: 56.1791140792, velocity: 18.0520286281, \
       index: 77}; { position: 55.2739006975, velocity: 18.104267635, \
       index: 76}; { position: 54.3665290512, velocity: 18.1474329256, \
       index: 75}; { position: 53.4574536781, velocity: 18.1815074623, \
       index: 74}; { position: 52.5471297401, velocity: 18.2064787597, \
       index: 73}; { position: 51.6360127957, velocity: 18.2223388877, \
       index: 72}; { position: 50.724558572, velocity: 18.2290844734, \
       index: 71}; { position: 49.813222737, velocity: 18.2267167008, \
       index: 70}; { position: 48.9024606716, velocity: 18.2152413075, \
       index: 69}; { position: 47.9927272426, velocity: 18.1946685799, \
       index: 68}; { position: 47.0844765753, velocity: 18.1650133457, \
       index: 67}; { position: 46.1781618271, velocity: 18.1262949639, \
       index: 66}; { position: 45.2742349614, velocity: 18.0785373136, \
       index: 65}; { position: 44.3731465225, velocity: 18.0217687788, \
       index: 64}; { position: 43.4753454109, velocity: 17.9560222329, \
       index: 63}; { position: 42.5812786599, velocity: 17.8813350195, \
       index: 62}; { position: 41.6913912133, velocity: 17.7977489316, \
       index: 61}; { position: 40.8061257039, velocity: 17.7053101887, \
       index: 60}; { position: 39.9259222333, velocity: 17.604069411, \
       index: 59}; { position: 39.0512181537, velocity: 17.4940815925, \
       index: 58}; { position: 38.1824478501, velocity: 17.375406071, \
       index: 57}; { position: 37.3200425253, velocity: 17.2481064963, \
       index: 56}; { position: 36.4644299855, velocity: 17.1122507961, \
       index: 55}; { position: 35.6160344285, velocity: 16.9679111404, \
       index: 54}; { position: 34.7752762334, velocity: 16.8151639028, \
       index: 53}; { position: 33.9425717523, velocity: 16.6540896203, \
       index: 52}; { position: 33.1183331048, velocity: 16.4847729513, \
       index: 51}; { position: 32.3029679732, velocity: 16.3073026311, \
       index: 50}; { position: 31.496879402, velocity: 16.1217714251, \
       index: 49}; { position: 30.7004655979, velocity: 15.9282760811, \
       index: 48}; { position: 29.914119734, velocity: 15.7269172784, \
       index: 47}; { position: 29.1382297552, velocity: 15.517799576, \
       index: 46}; { position: 28.3731781873, velocity: 15.3010313578, \
       index: 45}; { position: 27.6193419484, velocity: 15.0767247773, \
       index: 44}; { position: 26.8770921635, velocity: 14.844995699, \
       index: 43}; { position: 26.1467939816, velocity: 14.6059636388, \
       index: 42}; { position: 25.4288063964, velocity: 14.3597517027, \
       index: 41}; { position: 24.7234820702, velocity: 14.1064865234, \
       index: 40}; { position: 24.0311671605, velocity: 13.846298195, \
       index: 39}; { position: 23.3522011502, velocity: 13.5793202065, \
       index: 38}; { position: 22.6869166815, velocity: 13.3056893734, \
       index: 37}; { position: 22.0356393931, velocity: 13.0255457673, \
       index: 36}; { position: 21.3986877609, velocity: 12.7390326449, \
       index: 35}; { position: 20.7763729422, velocity: 12.4462963743, \
       index: 34}; { position: 20.1689986241, velocity: 12.1474863606, \
       index: 33}; { position: 19.5768608757, velocity: 11.8427549693, \
       index: 32}; { position: 19.0002480032, velocity: 11.5322574494, \
       index: 31}; { position: 18.4394404105, velocity: 11.2161518535, \
       index: 30}; { position: 17.8947104626, velocity: 10.8945989581, \
       index: 29}; { position: 17.3663223536, velocity: 10.5677621816, \
       index: 28}; { position: 16.8545319785, velocity: 10.2358075014, \
       index: 27}; { position: 16.35958681, velocity: 9.8989033695, \
       index: 26}; { position: 15.8817257786, velocity: 9.55722062729, \
       index: 25}; { position: 15.4211791577, velocity: 9.21093241887, \
       index: 24}; { position: 14.9781684525, velocity: 8.86021410339, \
       index: 23}; { position: 14.5529062942, velocity: 8.50524316633, \
       index: 22}; { position: 14.1455963377, velocity: 8.14619912971, \
       index: 21}; { position: 13.7564331647, velocity: 7.78326346136, \
       index: 20}; { position: 13.3856021905, velocity: 7.41661948326, \
       index: 19}; { position: 13.0332795765, velocity: 7.04645227903, \
       index: 18}; { position: 12.6996321465, velocity: 6.67294860049, \
       index: 17}; { position: 12.3848173078, velocity: 6.29629677357, \
       index: 16}; { position: 12.0889829777, velocity: 5.91668660335, \
       index: 15}; { position: 11.8122675138, velocity: 5.53430927849, \
       index: 14}; { position: 11.55479965, velocity: 5.14935727499, \
       index: 13}; { position: 11.316698437, velocity: 4.76202425936, \
       index: 12}; { position: 11.0980731875, velocity: 4.37250499123, \
       index: 11}; { position: 10.8990234262, velocity: 3.98099522549, \
       index: 10}; { position: 10.7196388455, velocity: 3.58769161395, \
       index: 9}; { position: 10.5599992652, velocity: 3.1927916066, \
       index: 8}; { position: 10.4201745975, velocity: 2.79649335257, \
       index: 7}; { position: 10.3002248175, velocity: 2.39899560075, \
       index: 6}; { position: 10.2001999375, velocity: 2.00049760013, \
       index: 5}; { position: 10.1201399875, velocity: 1.601199, \
       index: 4}; { position: 10.060075, velocity: 1.20129975, index: \
       3}; { position: 10.020025, velocity: 0.801, index: 2}; { \
       position: 10., velocity: 0.4005, index: 1}; { position: 10., \
       velocity: 0., index: 0}]";
  ]

let twod_to_string (state : Twodspring.t) =
  "{ " ^ "positionx: "
  ^ string_of_float (Twodspring.get_posx state)
  ^ ", " ^ "positiony: "
  ^ string_of_float (Twodspring.get_posy state)
  ^ ", velocityx: "
  ^ string_of_float (Twodspring.get_velx state)
  ^ ", velocityy: "
  ^ string_of_float (Twodspring.get_vely state)
  ^ "}"

let org2dbasic = Twodspring.make_state 1. 1. 1. 1.
let highy2d = Twodspring.make_state 1. 5. 1. 1.
let lowery2d = Twodspring.make_state 1. (-5.) 1. 1.
let highx2d = Twodspring.make_state 10. 1. 1. 1.
let lowx2d = Twodspring.make_state (-10.) 1. 1. 1.

let posx_test name twod expected_output =
  name >:: fun _ ->
  assert_equal expected_output
    (Twodspring.get_posx twod)
    ~printer:string_of_float

let posy_test name twod expected_output =
  name >:: fun _ ->
  assert_equal expected_output
    (Twodspring.get_posy twod)
    ~printer:string_of_float

let velx_test name twod expected_output =
  name >:: fun _ ->
  assert_equal expected_output
    (Twodspring.get_velx twod)
    ~printer:string_of_float

let vely_test name twod expected_output =
  name >:: fun _ ->
  assert_equal expected_output
    (Twodspring.get_vely twod)
    ~printer:string_of_float

let make_2d_test name x1 y1 x2 y2 expected_output =
  name >:: fun _ ->
  assert_equal expected_output
    (twod_to_string (Twodspring.make_state x1 y1 x2 y2))
    ~printer:pp_string

let next_2d_test name twodspring k m l dt expected_output =
  name >:: fun _ ->
  assert_equal expected_output
    (twod_to_string
       (Twodspring.compute_next_2d_state twodspring k m l 9.81 dt))
    ~printer:pp_string

let generate_2d_test name init k m l dt acc expected_output =
  name >:: fun _ ->
  assert_equal expected_output
    (pp_list twod_to_string
       (Twodspring.generate_states init k m l 9.81 dt 0 100 []))
    ~printer:pp_string

let twod_spring_tests =
  [
    posx_test "basic test" org2dbasic 1.;
    posx_test "lowerx" lowx2d (-10.);
    posx_test "highx" highx2d 10.;
    posx_test "highy" highy2d 1.;
    posx_test "lowery" lowery2d 1.;
    posy_test "basic test" org2dbasic 1.;
    posy_test "lowerx" lowx2d 1.;
    posy_test "highx" highx2d 1.;
    posy_test "highy" highy2d 5.;
    posy_test "lowery" lowery2d (-5.);
    velx_test "basic test" org2dbasic 1.;
    velx_test "lowerx" lowx2d 1.;
    velx_test "highx" highx2d 1.;
    velx_test "highy" highy2d 1.;
    velx_test "lowery" lowery2d 1.;
    vely_test "basic test" org2dbasic 1.;
    vely_test "lowerx" lowx2d 1.;
    vely_test "highx" highx2d 1.;
    vely_test "highy" highy2d 1.;
    vely_test "lowery" lowery2d 1.;
    make_2d_test "origin" 0. 0. 0. 0.
      "{ positionx: 0., positiony: 0., velocityx: 0., velocityy: 0.}";
    make_2d_test "higher y" 0. 5. 0. 0.
      "{ positionx: 0., positiony: 5., velocityx: 0., velocityy: 0.}";
    make_2d_test "lower y" 0. (-5.) 0. 0.
      "{ positionx: 0., positiony: -5., velocityx: 0., velocityy: 0.}";
    make_2d_test "higher x" 5. 0. 0. 0.
      "{ positionx: 5., positiony: 0., velocityx: 0., velocityy: 0.}";
    make_2d_test "lower x" (-5.) 0. 0. 0.
      "{ positionx: -5., positiony: 0., velocityx: 0., velocityy: 0.}";
    next_2d_test "origin next state" org2dbasic 5. 5. 1. 0.03
      "{ positionx: 1.03, positiony: 1.03, velocityx: 0.991213203436, \
       velocityy: 1.28551320344}";
    next_2d_test "highy next state" highy2d 1. 1. 1. 0.03
      "{ positionx: 1.03, positiony: 5.03, velocityx: 0.975883484054, \
       velocityy: 1.17371742027}";
    next_2d_test "lowy next state" lowery2d 1. 1. 1. 0.02
      "{ positionx: 1.02, positiony: -4.98, velocityx: 0.983922322703, \
       velocityy: 1.27658838649}";
    next_2d_test "lowx next state" lowx2d 1. 1. 1. 0.02
      "{ positionx: -9.98, positiony: 1.02, velocityx: 1.1800992562, \
       velocityy: 1.17819007438}";
    next_2d_test "highx next state" highx2d 1. 1. 1. 0.02
      "{ positionx: 10.02, positiony: 1.02, velocityx: 0.819900743804, \
       velocityy: 1.17819007438}";
    generate_2d_test "org" org2dbasic 1. 1. 1. 0.01 []
      "[{ positionx: 1., positiony: 1., velocityx: 1., velocityy: 1.}; \
       { positionx: 1.01, positiony: 1.01, velocityx: 0.997071067812, \
       velocityy: 1.09517106781}; { positionx: 1.01997071068, \
       positiony: 1.02095171068, velocityx: 0.994042135624, velocityy: \
       1.19024213562}; { positionx: 1.02991113203, positiony: \
       1.03285413203, velocityx: 0.990910096697, velocityy: \
       1.28520708433}; { positionx: 1.039820233, positiony: \
       1.04570620288, velocityx: 0.987671957529, velocityy: \
       1.38005969208}; { positionx: 1.04969695258, positiony: \
       1.0595067998, velocityx: 0.984324838293, velocityy: \
       1.47479362626}; { positionx: 1.05954020096, positiony: \
       1.07425473606, velocityx: 0.98086597312, velocityy: \
       1.56940243658}; { positionx: 1.06934886069, positiony: \
       1.08994876043, velocityx: 0.977292710255, velocityy: \
       1.66387954945}; { positionx: 1.07912178779, positiony: \
       1.10658755592, velocityx: 0.973602512127, velocityy: \
       1.75821826347}; { positionx: 1.08885781291, positiony: \
       1.12416973856, velocityx: 0.969792955354, velocityy: \
       1.852411746}; { positionx: 1.09855574247, positiony: \
       1.14269385602, velocityx: 0.965861730701, velocityy: \
       1.94645303077}; { positionx: 1.10821435978, positiony: \
       1.16215838632, velocityx: 0.961806643001, velocityy: \
       2.0403350165}; { positionx: 1.11783242621, positiony: \
       1.18256173649, velocityx: 0.957625611045, velocityy: \
       2.13405046643}; { positionx: 1.12740868232, positiony: \
       1.20390224115, velocityx: 0.953316667434, velocityy: \
       2.22759200872}; { positionx: 1.13694184899, positiony: \
       1.22617816124, velocityx: 0.948877958387, velocityy: \
       2.32095213767}; { positionx: 1.14643062857, positiony: \
       1.24938768262, velocityx: 0.944307743506, velocityy: \
       2.41412321568}; { positionx: 1.15587370601, positiony: \
       1.27352891477, velocityx: 0.939604395482, velocityy: \
       2.5070974759}; { positionx: 1.16526974996, positiony: \
       1.29859988953, velocityx: 0.934766399718, velocityy: \
       2.59986702546}; { positionx: 1.17461741396, positiony: \
       1.32459855979, velocityx: 0.929792353885, velocityy: \
       2.69242384942}; { positionx: 1.1839153375, positiony: \
       1.35152279828, velocityx: 0.924680967367, velocityy: \
       2.78475981498}; { positionx: 1.19316214717, positiony: \
       1.37937039643, velocityx: 0.919431060605, velocityy: \
       2.87686667641}; { positionx: 1.20235645778, positiony: \
       1.4081390632, velocityx: 0.914041564323, velocityy: \
       2.96873608014}; { positionx: 1.21149687342, positiony: \
       1.437826424, velocityx: 0.908511518623, velocityy: \
       3.06035957035}; { positionx: 1.22058198861, positiony: \
       1.4684300197, velocityx: 0.902840071944, velocityy: \
       3.15172859474}; { positionx: 1.22961038933, positiony: \
       1.49994730565, velocityx: 0.897026479887, velocityy: \
       3.24283451063}; { positionx: 1.23858065413, positiony: \
       1.53237565075, velocityx: 0.891070103887, velocityy: \
       3.3336685911}; { positionx: 1.24749135517, positiony: \
       1.56571233667, velocityx: 0.88497040974, velocityy: \
       3.42422203146}; { positionx: 1.25634105926, positiony: \
       1.59995455698, velocityx: 0.878726965994, velocityy: \
       3.51448595559}; { positionx: 1.26512832892, positiony: \
       1.63509941654, velocityx: 0.872339442183, velocityy: \
       3.60445142258}; { positionx: 1.27385172335, positiony: \
       1.67114393076, velocityx: 0.865807606937, velocityy: \
       3.69410943313}; { positionx: 1.28250979941, positiony: \
       1.70808502509, velocityx: 0.859131325943, velocityy: \
       3.78345093616}; { positionx: 1.29110111267, positiony: \
       1.74591953445, velocityx: 0.852310559797, velocityy: \
       3.87246683519}; { positionx: 1.29962421827, positiony: \
       1.78464420281, velocityx: 0.84534536173, velocityy: \
       3.96114799475}; { positionx: 1.30807767189, positiony: \
       1.82425568275, velocityx: 0.83823587523, velocityy: \
       4.04948524665}; { positionx: 1.31646003064, positiony: \
       1.86475053522, velocityx: 0.830982331571, velocityy: \
       4.1374693961}; { positionx: 1.32476985396, positiony: \
       1.90612522918, velocityx: 0.823585047254, velocityy: \
       4.2250912277}; { positionx: 1.33300570443, positiony: \
       1.94837614146, velocityx: 0.816044421381, velocityy: \
       4.31234151129}; { positionx: 1.34116614864, positiony: \
       1.99149955657, velocityx: 0.808360932966, velocityy: \
       4.39921100758}; { positionx: 1.34924975797, positiony: \
       2.03549166665, velocityx: 0.8005351382, velocityy: \
       4.4856904736}; { positionx: 1.35725510936, positiony: \
       2.08034857138, velocityx: 0.792567667684, velocityy: \
       4.57177066797}; { positionx: 1.36518078603, positiony: \
       2.12606627806, velocityx: 0.784459223631, velocityy: \
       4.65744235594}; { positionx: 1.37302537827, positiony: \
       2.17264070162, velocityx: 0.776210577067, velocityy: \
       4.74269631424}; { positionx: 1.38078748404, positiony: \
       2.22006766476, velocityx: 0.767822565025, velocityy: \
       4.82752333571}; { positionx: 1.38846570969, positiony: \
       2.26834289812, velocityx: 0.759296087755, velocityy: \
       4.9119142337}; { positionx: 1.39605867057, positiony: \
       2.31746204046, velocityx: 0.750632105945, velocityy: \
       4.99585984625}; { positionx: 1.40356499163, positiony: \
       2.36742063892, velocityx: 0.741831637985, velocityy: \
       5.07935104014}; { positionx: 1.41098330801, positiony: \
       2.41821414932, velocityx: 0.732895757257, velocityy: \
       5.16237871461}; { positionx: 1.41831226558, positiony: \
       2.46983793647, velocityx: 0.723825589477, velocityy: \
       5.24493380494}; { positionx: 1.42555052147, positiony: \
       2.52228727452, velocityx: 0.714622310091, velocityy: \
       5.32700728588}; { positionx: 1.43269674457, positiony: \
       2.57555734738, velocityx: 0.705287141722, velocityy: \
       5.40859017475}; { positionx: 1.43974961599, positiony: \
       2.62964324912, velocityx: 0.695821351689, velocityy: \
       5.48967353451}; { positionx: 1.44670782951, positiony: \
       2.68453998447, velocityx: 0.68622624959, velocityy: \
       5.57024847656}; { positionx: 1.453570092, positiony: \
       2.74024246923, velocityx: 0.676503184958, velocityy: \
       5.65030616335}; { positionx: 1.46033512385, positiony: \
       2.79674553087, velocityx: 0.666653544989, velocityy: \
       5.72983781092}; { positionx: 1.4670016593, positiony: \
       2.85404390898, velocityx: 0.656678752356, velocityy: \
       5.80883469119}; { positionx: 1.47356844683, positiony: \
       2.91213225589, velocityx: 0.64658026309, velocityy: \
       5.88728813415}; { positionx: 1.48003424946, positiony: \
       2.97100513723, velocityx: 0.636359564552, velocityy: \
       5.9651895299}; { positionx: 1.4863978451, positiony: \
       3.03065703253, velocityx: 0.626018173479, velocityy: \
       6.04253033057}; { positionx: 1.49265802684, positiony: \
       3.09108233584, velocityx: 0.615557634113, velocityy: \
       6.11930205208}; { positionx: 1.49881360318, positiony: \
       3.15227535636, velocityx: 0.604979516415, velocityy: \
       6.19549627582}; { positionx: 1.50486339834, positiony: \
       3.21423031911, velocityx: 0.594285414347, velocityy: \
       6.27110465022}; { positionx: 1.51080625249, positiony: \
       3.27694136562, velocityx: 0.583476944251, velocityy: \
       6.34611889219}; { positionx: 1.51664102193, positiony: \
       3.34040255454, velocityx: 0.572555743287, velocityy: \
       6.42053078847}; { positionx: 1.52236657936, positiony: \
       3.40460786242, velocityx: 0.561523467961, velocityy: \
       6.49433219695}; { positionx: 1.52798181404, positiony: \
       3.46955118439, velocityx: 0.550381792717, velocityy: \
       6.56751504779}; { positionx: 1.53348563197, positiony: \
       3.53522633487, velocityx: 0.539132408611, velocityy: \
       6.64007134459}; { positionx: 1.53887695606, positiony: \
       3.60162704832, velocityx: 0.527777022045, velocityy: \
       6.71199316542}; { positionx: 1.54415472628, positiony: \
       3.66874697997, velocityx: 0.516317353575, velocityy: \
       6.78327266378}; { positionx: 1.54931789981, positiony: \
       3.73657970661, velocityx: 0.504755136785, velocityy: \
       6.85390206954}; { positionx: 1.55436545118, positiony: \
       3.8051187273, velocityx: 0.493092117218, velocityy: \
       6.92387368979}; { positionx: 1.55929637235, positiony: \
       3.8743574642, velocityx: 0.48133005137, velocityy: \
       6.99317990967}; { positionx: 1.56410967287, positiony: \
       3.9442892633, velocityx: 0.469470705747, velocityy: \
       7.06181319311}; { positionx: 1.56880437992, positiony: \
       4.01490739523, velocityx: 0.457515855967, velocityy: \
       7.12976608358}; { positionx: 1.57337953848, positiony: \
       4.08620505607, velocityx: 0.445467285925, velocityy: \
       7.19703120475}; { positionx: 1.57783421134, positiony: \
       4.15817536811, velocityx: 0.433326787, velocityy: \
       7.26360126116}; { positionx: 1.58216747921, positiony: \
       4.23081138072, velocityx: 0.421096157314, velocityy: \
       7.32946903883}; { positionx: 1.58637844079, positiony: \
       4.30410607111, velocityx: 0.408777201036, velocityy: \
       7.39462740585}; { positionx: 1.5904662128, positiony: \
       4.37805234517, velocityx: 0.396371727727, velocityy: \
       7.45906931291}; { positionx: 1.59442993007, positiony: \
       4.4526430383, velocityx: 0.383881551727, velocityy: \
       7.5227877939}; { positionx: 1.59826874559, positiony: \
       4.52787091624, velocityx: 0.371308491578, velocityy: \
       7.58577596636}; { positionx: 1.60198183051, positiony: \
       4.6037286759, velocityx: 0.358654369491, velocityy: \
       7.64802703199}; { positionx: 1.6055683742, positiony: \
       4.68020894622, velocityx: 0.345921010832, velocityy: \
       7.70953427714}; { positionx: 1.60902758431, positiony: \
       4.75730428899, velocityx: 0.333110243657, velocityy: \
       7.77029107326}; { positionx: 1.61235868675, positiony: \
       4.83500719973, velocityx: 0.320223898265, velocityy: \
       7.83029087733}; { positionx: 1.61556092573, positiony: \
       4.9133101085, velocityx: 0.307263806781, velocityy: \
       7.88952723232}; { positionx: 1.6186335638, positiony: \
       4.99220538082, velocityx: 0.294231802774, velocityy: \
       7.94799376756}; { positionx: 1.62157588182, positiony: \
       5.0716853185, velocityx: 0.281129720886, velocityy: \
       8.00568419917}; { positionx: 1.62438717903, positiony: \
       5.15174216049, velocityx: 0.267959396499, velocityy: \
       8.06259233045}; { positionx: 1.627066773, positiony: \
       5.2323680838, velocityx: 0.254722665406, velocityy: \
       8.11871205228}; { positionx: 1.62961399965, positiony: \
       5.31355520432, velocityx: 0.241421363521, velocityy: \
       8.17403734345}; { positionx: 1.63202821329, positiony: \
       5.39529557775, velocityx: 0.228057326593, velocityy: \
       8.22856227108}; { positionx: 1.63430878655, positiony: \
       5.47758120046, velocityx: 0.214632389945, velocityy: \
       8.28228099094}; { positionx: 1.63645511045, positiony: \
       5.56040401037, velocityx: 0.201148388225, velocityy: \
       8.33518774782}; { positionx: 1.63846659433, positiony: \
       5.64375588785, velocityx: 0.187607155177, velocityy: \
       8.38727687587}; { positionx: 1.64034266589, positiony: \
       5.72762865661, velocityx: 0.174010523419, velocityy: \
       8.43854279893}; { positionx: 1.64208277112, positiony: \
       5.8120140846, velocityx: 0.16036032424, velocityy: \
       8.48898003089}; { positionx: 1.64368637436, positiony: \
       5.89690388491, velocityx: 0.146658387403, velocityy: \
       8.53858317599}; { positionx: 1.64515295824, positiony: \
       5.98228971667, velocityx: 0.132906540966, velocityy: \
       8.58734692916}; { positionx: 1.64648202365, positiony: \
       6.06816318596, velocityx: 0.119106611106, velocityy: \
       8.63526607633}; { positionx: 1.64767308976, positiony: \
       6.15451584672, velocityx: 0.105260421959, velocityy: \
       8.68233549473}; { positionx: 1.64872569398, positiony: \
       6.24133920167, velocityx: 0.0913697954573, velocityy: \
       8.72855015324}]";
    generate_2d_test "lowerx" lowx2d 1. 1. 1. 0.01 []
      "[{ positionx: -10., positiony: 1., velocityx: 1., velocityy: \
       1.}; { positionx: -9.99, positiony: 1.01, velocityx: \
       1.0900496281, velocityy: 1.08909503719}; { positionx: \
       -9.97909950372, positiony: 1.02089095037, velocityx: \
       1.18000034678, velocityy: 1.17810092049}; { positionx: \
       -9.96729950025, positiony: 1.03267195958, velocityx: \
       1.26984326404, velocityy: 1.26700972832}; { positionx: \
       -9.95460106761, positiony: 1.04534205686, velocityx: \
       1.35956950179, velocityy: 1.35581355238}; { positionx: \
       -9.94100537259, positiony: 1.05890019238, velocityx: \
       1.44917019711, velocityy: 1.44450449877}; { positionx: \
       -9.92651367062, positiony: 1.07334523737, velocityx: \
       1.53863650346, velocityy: 1.53307468911}; { positionx: \
       -9.91112730559, positiony: 1.08867598426, velocityx: \
       1.62795959202, velocityy: 1.62151626171}; { positionx: \
       -9.89484770967, positiony: 1.10489114688, velocityx: \
       1.7171306529, velocityy: 1.70982137264}; { positionx: \
       -9.87767640314, positiony: 1.12198936061, velocityx: \
       1.80614089642, velocityy: 1.79798219693}; { positionx: \
       -9.85961499417, positiony: 1.13996918258, velocityx: \
       1.89498155444, velocityy: 1.88599092961}; { positionx: \
       -9.84066517863, positiony: 1.15882909187, velocityx: \
       1.9836438816, velocityy: 1.97383978687}; { positionx: \
       -9.82082873981, positiony: 1.17856748974, velocityx: \
       2.07211915667, velocityy: 2.06152100717}; { positionx: \
       -9.80010754825, positiony: 1.19918269981, velocityx: \
       2.16039868382, velocityy: 2.14902685229}; { positionx: \
       -9.77850356141, positiony: 1.22067296833, velocityx: \
       2.24847379398, velocityy: 2.23634960848}; { positionx: \
       -9.75601882347, positiony: 1.24303646442, velocityx: \
       2.33633584614, velocityy: 2.32348158752}; { positionx: \
       -9.73265546501, positiony: 1.26627128029, velocityx: \
       2.42397622871, velocityy: 2.41041512779}; { positionx: \
       -9.70841570272, positiony: 1.29037543157, velocityx: \
       2.51138636089, velocityy: 2.49714259535}; { positionx: \
       -9.68330183911, positiony: 1.31534685753, velocityx: \
       2.598557694, velocityy: 2.58365638499}; { positionx: \
       -9.65731626217, positiony: 1.34118342138, velocityx: \
       2.6854817129, velocityy: 2.66994892128}; { positionx: \
       -9.63046144504, positiony: 1.36788291059, velocityx: \
       2.77214993735, velocityy: 2.75601265959}; { positionx: \
       -9.60273994567, positiony: 1.39544303718, velocityx: \
       2.85855392342, velocityy: 2.84184008712}; { positionx: \
       -9.57415440643, positiony: 1.42386143806, velocityx: \
       2.94468526494, velocityy: 2.92742372393}; { positionx: \
       -9.54470755379, positiony: 1.4531356753, velocityx: \
       3.03053559488, velocityy: 3.01275612388}; { positionx: \
       -9.51440219784, positiony: 1.48326323653, velocityx: \
       3.11609658685, velocityy: 3.09782987566}; { positionx: \
       -9.48324123197, positiony: 1.51424153529, velocityx: \
       3.20135995653, velocityy: 3.18263760368}; { positionx: \
       -9.4512276324, positiony: 1.54606791133, velocityx: \
       3.28631746315, velocityy: 3.26717196909}; { positionx: \
       -9.41836445777, positiony: 1.57873963102, velocityx: \
       3.37096091102, velocityy: 3.35142567063}; { positionx: \
       -9.38465484866, positiony: 1.61225388772, velocityx: \
       3.45528215098, velocityy: 3.43539144554}; { positionx: \
       -9.35010202715, positiony: 1.64660780218, velocityx: \
       3.53927308196, velocityy: 3.51906207044}; { positionx: \
       -9.31470929633, positiony: 1.68179842288, velocityx: \
       3.62292565253, velocityy: 3.60243036217}; { positionx: \
       -9.27848003981, positiony: 1.71782272651, velocityx: \
       3.70623186243, velocityy: 3.68548917859}; { positionx: \
       -9.24141772118, positiony: 1.75467761829, velocityx: \
       3.78918376417, velocityy: 3.76823141941}; { positionx: \
       -9.20352588354, positiony: 1.79235993249, velocityx: \
       3.87177346461, velocityy: 3.85065002688}; { positionx: \
       -9.16480814889, positiony: 1.83086643276, velocityx: \
       3.95399312656, velocityy: 3.93273798654}; { positionx: \
       -9.12526821763, positiony: 1.87019381262, velocityx: \
       4.03583497041, velocityy: 4.0144883279}; { positionx: \
       -9.08490986792, positiony: 1.9103386959, velocityx: \
       4.11729127576, velocityy: 4.0958941251}; { positionx: \
       -9.04373695517, positiony: 1.95129763715, velocityx: \
       4.19835438309, velocityy: 4.17694849746}; { positionx: \
       -9.00175341134, positiony: 1.99306712212, velocityx: \
       4.27901669543, velocityy: 4.25764461008}; { positionx: \
       -8.95896324438, positiony: 2.03564356823, velocityx: \
       4.35927067999, velocityy: 4.33797567432}; { positionx: \
       -8.91537053758, positiony: 2.07902332497, velocityx: \
       4.43910886994, velocityy: 4.41793494828}; { positionx: \
       -8.87097944888, positiony: 2.12320267445, velocityx: \
       4.51852386603, velocityy: 4.49751573715}; { positionx: \
       -8.82579421022, positiony: 2.16817783182, velocityx: \
       4.59750833837, velocityy: 4.57671139363}; { positionx: \
       -8.77981912684, positiony: 2.21394494576, velocityx: \
       4.67605502813, velocityy: 4.65551531812}; { positionx: \
       -8.73305857656, positiony: 2.26050009894, velocityx: \
       4.75415674926, velocityy: 4.73392095901}; { positionx: \
       -8.68551700906, positiony: 2.30783930853, velocityx: \
       4.83180639025, velocityy: 4.81192181281}; { positionx: \
       -8.63719894516, positiony: 2.35595852666, velocityx: \
       4.90899691585, velocityy: 4.88951142419}; { positionx: \
       -8.588108976, positiony: 2.4048536409, velocityx: \
       4.98572136883, velocityy: 4.96668338602}; { positionx: \
       -8.53825176232, positiony: 2.45452047476, velocityx: \
       5.06197287171, velocityy: 5.04343133927}; { positionx: \
       -8.4876320336, positiony: 2.50495478815, velocityx: \
       5.13774462848, velocityy: 5.11974897286}; { positionx: \
       -8.43625458731, positiony: 2.55615227788, velocityx: \
       5.21302992631, velocityy: 5.19563002339}; { positionx: \
       -8.38412428805, positiony: 2.60810857812, velocityx: \
       5.28782213731, velocityy: 5.2710682748}; { positionx: \
       -8.33124606668, positiony: 2.66081926086, velocityx: \
       5.36211472015, velocityy: 5.34605755793}; { positionx: \
       -8.27762491948, positiony: 2.71427983644, velocityx: \
       5.43590122178, velocityy: 5.42059174999}; { positionx: \
       -8.22326590726, positiony: 2.76848575394, velocityx: \
       5.50917527904, velocityy: 5.49466477389}; { positionx: \
       -8.16817415447, positiony: 2.82343240168, velocityx: \
       5.58193062025, velocityy: 5.56827059749}; { positionx: \
       -8.11235484827, positiony: 2.87911510766, velocityx: \
       5.6541610668, velocityy: 5.64140323277}; { positionx: \
       -8.0558132376, positiony: 2.93552913998, velocityx: \
       5.72586053467, velocityy: 5.71405673481}; { positionx: \
       -7.99855463225, positiony: 2.99266970733, velocityx: \
       5.79702303582, velocityy: 5.78622520069}; { positionx: \
       -7.94058440189, positiony: 3.05053195934, velocityx: \
       5.86764267968, velocityy: 5.85790276831}; { positionx: \
       -7.8819079751, positiony: 3.10911098702, velocityx: \
       5.9377136744, velocityy: 5.92908361501}; { positionx: \
       -7.82253083835, positiony: 3.16840182317, velocityx: \
       6.00723032815, velocityy: 5.99976195611}; { positionx: \
       -7.76245853507, positiony: 3.22839944273, velocityx: \
       6.07618705023, velocityy: 6.06993204334}; { positionx: \
       -7.70169666457, positiony: 3.28909876317, velocityx: \
       6.14457835214, velocityy: 6.1395881631}; { positionx: \
       -7.64025088105, positiony: 3.3504946448, velocityx: \
       6.21239884855, velocityy: 6.2087246346}; { positionx: \
       -7.57812689256, positiony: 3.41258189114, velocityx: \
       6.2796432581, velocityy: 6.27733580791}; { positionx: \
       -7.51533045998, positiony: 3.47535524922, velocityx: \
       6.3463064041, velocityy: 6.34541606186}; { positionx: \
       -7.45186739594, positiony: 3.53880940984, velocityx: \
       6.41238321509, velocityy: 6.4129598018}; { positionx: \
       -7.38774356379, positiony: 3.60293900786, velocityx: \
       6.47786872524, velocityy: 6.47996145729}; { positionx: \
       -7.32296487654, positiony: 3.66773862243, velocityx: \
       6.54275807456, velocityy: 6.54641547962}; { positionx: \
       -7.25753729579, positiony: 3.73320277723, velocityx: \
       6.60704650898, velocityy: 6.61231633926}; { positionx: \
       -7.1914668307, positiony: 3.79932594062, velocityx: \
       6.6707293802, velocityy: 6.67765852321}; { positionx: \
       -7.1247595369, positiony: 3.86610252585, velocityx: \
       6.73380214536, velocityy: 6.74243653224}; { positionx: \
       -7.05742151544, positiony: 3.93352689118, velocityx: \
       6.79626036648, velocityy: 6.80664487804}; { positionx: \
       -6.98945891178, positiony: 4.00159333996, velocityx: \
       6.8580997097, velocityy: 6.87027808037}; { positionx: \
       -6.92087791468, positiony: 4.07029612076, velocityx: \
       6.91931594424, velocityy: 6.93333066404}; { positionx: \
       -6.85168475524, positiony: 4.1396294274, velocityx: \
       6.97990494118, velocityy: 6.99579715594}; { positionx: \
       -6.78188570583, positiony: 4.20958739896, velocityx: \
       7.03986267196, velocityy: 7.05767208202}; { positionx: \
       -6.71148707911, positiony: 4.28016411978, velocityx: \
       7.09918520656, velocityy: 7.11894996425}; { positionx: \
       -6.64049522704, positiony: 4.35135361942, velocityx: \
       7.15786871153, velocityy: 7.17962531761}; { positionx: \
       -6.56891653993, positiony: 4.4231498726, velocityx: \
       7.21590944766, velocityy: 7.23969264707}; { positionx: \
       -6.49675744545, positiony: 4.49554679907, velocityx: \
       7.27330376739, velocityy: 7.29914644469}; { positionx: \
       -6.42402440778, positiony: 4.56853826352, velocityx: \
       7.33004811198, velocityy: 7.35798118677}; { positionx: \
       -6.35072392666, positiony: 4.64211807538, velocityx: \
       7.3861390084, velocityy: 7.41619133109}; { positionx: \
       -6.27686253657, positiony: 4.7162799887, velocityx: \
       7.44157306588, velocityy: 7.47377131429}; { positionx: \
       -6.20244680591, positiony: 4.79101770184, velocityx: \
       7.49634697233, velocityy: 7.53071554939}; { positionx: \
       -6.12748333619, positiony: 4.86632485733, velocityx: \
       7.55045749034, velocityy: 7.58701842355}; { positionx: \
       -6.05197876129, positiony: 4.94219504157, velocityx: \
       7.60390145312, velocityy: 7.64267429596}; { positionx: \
       -5.97593974676, positiony: 5.01862178453, velocityx: \
       7.65667576002, velocityy: 7.69767749602}; { positionx: \
       -5.89937298916, positiony: 5.09559855949, velocityx: \
       7.70877737201, velocityy: 7.75202232176}; { positionx: \
       -5.82228521544, positiony: 5.17311878271, velocityx: \
       7.76020330686, velocityy: 7.80570303856}; { positionx: \
       -5.74468318237, positiony: 5.25117581309, velocityx: \
       7.81095063416, velocityy: 7.85871387815}; { positionx: \
       -5.66657367603, positiony: 5.32976295187, velocityx: \
       7.86101647026, velocityy: 7.91104903799}; { positionx: \
       -5.58796351132, positiony: 5.40887344225, velocityx: \
       7.91039797303, velocityy: 7.96270268099}; { positionx: \
       -5.50885953159, positiony: 5.48850046906, velocityx: \
       7.95909233654, velocityy: 8.01366893557}; { positionx: \
       -5.42926860823, positiony: 5.56863715842, velocityx: \
       8.00709678574, velocityy: 8.06394189615}; { positionx: \
       -5.34919764037, positiony: 5.64927657738, velocityx: \
       8.05440857106, velocityy: 8.11351562405}; { positionx: \
       -5.26865355466, positiony: 5.73041173362, velocityx: \
       8.10102496308, velocityy: 8.16238414875}; { positionx: \
       -5.18764330503, positiony: 5.81203557511, velocityx: \
       8.14694324717, velocityy: 8.21054146961}; { positionx: \
       -5.10617387256, positiony: 5.8941409898, velocityx: \
       8.19216071837, velocityy: 8.25798155801}; { positionx: \
       -5.02425226537, positiony: 5.97672080538, velocityx: \
       8.23667467622, velocityy: 8.30469835992}]";
    generate_2d_test "highery" highy2d 1. 1. 1. 0.01 []
      "[{ positionx: 1., positiony: 5., velocityx: 1., velocityy: 1.}; \
       { positionx: 1.01, positiony: 5.01, velocityx: 0.991961161351, \
       velocityy: 1.05790580676}; { positionx: 1.01991961161, \
       positiony: 5.02057905807, velocityx: 0.983837371498, velocityy: \
       1.11570859174}; { positionx: 1.02975798533, positiony: \
       5.03173614398, velocityx: 0.975628989247, velocityy: \
       1.17340263076}; { positionx: 1.03951427522, positiony: \
       5.04347017029, velocityx: 0.967336379476, velocityy: \
       1.23098221246}; { positionx: 1.04918763902, positiony: \
       5.05577999242, velocityx: 0.958959913464, velocityy: \
       1.28844163881}; { positionx: 1.05877723815, positiony: \
       5.06866440881, velocityx: 0.950499969203, velocityy: \
       1.3457752255}; { positionx: 1.06828223784, positiony: \
       5.08212216106, velocityx: 0.94195693172, velocityy: \
       1.40297730243}; { positionx: 1.07770180716, positiony: \
       5.09615193408, velocityx: 0.933331193383, velocityy: \
       1.46004221413}; { positionx: 1.08703511909, positiony: \
       5.11075235623, velocityx: 0.924623154206, velocityy: \
       1.51696432021}; { positionx: 1.09628135064, positiony: \
       5.12592199943, velocityx: 0.915833222141, velocityy: \
       1.57373799577}; { positionx: 1.10543968286, positiony: \
       5.14165937939, velocityx: 0.906961813372, velocityy: \
       1.63035763186}; { positionx: 1.11450930099, positiony: \
       5.1579629557, velocityx: 0.89800935259, velocityy: \
       1.68681763588}; { positionx: 1.12348939452, positiony: \
       5.17483113206, velocityx: 0.888976273271, velocityy: \
       1.74311243204}; { positionx: 1.13237915725, positiony: \
       5.19226225638, velocityx: 0.879863017936, velocityy: \
       1.79923646177}; { positionx: 1.14117778743, positiony: \
       5.210254621, velocityx: 0.870670038408, velocityy: \
       1.85518418414}; { positionx: 1.14988448781, positiony: \
       5.22880646284, velocityx: 0.861397796059, velocityy: \
       1.91095007627}; { positionx: 1.15849846577, positiony: \
       5.24791596361, velocityx: 0.852046762045, velocityy: \
       1.96652863381}; { positionx: 1.16701893339, positiony: \
       5.26758124994, velocityx: 0.842617417531, velocityy: \
       2.02191437128}; { positionx: 1.17544510757, positiony: \
       5.28780039366, velocityx: 0.83311025391, velocityy: \
       2.07710182257}; { positionx: 1.18377621011, positiony: \
       5.30857141188, velocityx: 0.82352577301, velocityy: \
       2.1320855413}; { positionx: 1.19201146784, positiony: \
       5.32989226729, velocityx: 0.813864487289, velocityy: \
       2.18686010129}; { positionx: 1.20015011271, positiony: \
       5.35176086831, velocityx: 0.804126920018, velocityy: \
       2.24142009693}; { positionx: 1.20819138191, positiony: \
       5.37417506928, velocityx: 0.794313605463, velocityy: \
       2.29576014368}; { positionx: 1.21613451797, positiony: \
       5.39713267071, velocityx: 0.784425089041, velocityy: \
       2.34987487842}; { positionx: 1.22397876886, positiony: \
       5.4206314195, velocityx: 0.77446192748, velocityy: \
       2.40375895989}; { positionx: 1.23172338813, positiony: \
       5.4446690091, velocityx: 0.76442468896, velocityy: \
       2.45740706916}; { positionx: 1.23936763502, positiony: \
       5.46924307979, velocityx: 0.754313953244, velocityy: \
       2.51081391001}; { positionx: 1.24691077455, positiony: \
       5.49435121889, velocityx: 0.744130311801, velocityy: \
       2.56397420938}; { positionx: 1.25435207767, positiony: \
       5.51999096098, velocityx: 0.733874367916, velocityy: \
       2.61688271779}; { positionx: 1.26169082135, positiony: \
       5.54615978816, velocityx: 0.723546736791, velocityy: \
       2.66953420977}; { positionx: 1.26892628872, positiony: \
       5.57285513026, velocityx: 0.713148045635, velocityy: \
       2.72192348432}; { positionx: 1.27605776917, positiony: \
       5.6000743651, velocityx: 0.702678933739, velocityy: \
       2.77404536529}; { positionx: 1.28308455851, positiony: \
       5.62781481875, velocityx: 0.692140052551, velocityy: \
       2.82589470188}; { positionx: 1.29000595904, positiony: \
       5.65607376577, velocityx: 0.68153206573, velocityy: \
       2.87746636902}; { positionx: 1.29682127969, positiony: \
       5.68484842946, velocityx: 0.670855649197, velocityy: \
       2.92875526785}; { positionx: 1.30352983619, positiony: \
       5.71413598214, velocityx: 0.660111491172, velocityy: \
       2.97975632612}; { positionx: 1.3101309511, positiony: \
       5.7439335454, velocityx: 0.649300292202, velocityy: \
       3.0304644987}; { positionx: 1.31662395402, positiony: \
       5.77423819039, velocityx: 0.638422765183, velocityy: \
       3.08087476792}; { positionx: 1.32300818167, positiony: \
       5.80504693807, velocityx: 0.627479635367, velocityy: \
       3.13098214413}; { positionx: 1.32928297803, positiony: \
       5.83635675951, velocityx: 0.616471640363, velocityy: \
       3.18078166604}; { positionx: 1.33544769443, positiony: \
       5.86816457617, velocityx: 0.605399530131, velocityy: \
       3.23026840124}; { positionx: 1.34150168973, positiony: \
       5.90046726018, velocityx: 0.594264066959, velocityy: \
       3.27943744663}; { positionx: 1.3474443304, positiony: \
       5.93326163465, velocityx: 0.583066025443, velocityy: \
       3.32828392884}; { positionx: 1.35327499065, positiony: \
       5.96654447394, velocityx: 0.57180619245, velocityy: \
       3.37680300471}; { positionx: 1.35899305258, positiony: \
       6.00031250399, velocityx: 0.560485367075, velocityy: \
       3.42498986175}; { positionx: 1.36459790625, positiony: \
       6.0345624026, velocityx: 0.549104360591, velocityy: \
       3.47283971854}; { positionx: 1.37008894986, positiony: \
       6.06929079979, velocityx: 0.537663996392, velocityy: \
       3.52034782524}; { positionx: 1.37546558982, positiony: \
       6.10449427804, velocityx: 0.526165109924, velocityy: \
       3.56750946401}; { positionx: 1.38072724092, positiony: \
       6.14016937268, velocityx: 0.514608548618, velocityy: \
       3.61431994947}; { positionx: 1.38587332641, positiony: \
       6.17631257217, velocityx: 0.502995171804, velocityy: \
       3.66077462915}; { positionx: 1.39090327812, positiony: \
       6.21292031847, velocityx: 0.491325850628, velocityy: \
       3.70686888395}; { positionx: 1.39581653663, positiony: \
       6.24998900731, velocityx: 0.479601467958, velocityy: \
       3.75259812859}; { positionx: 1.40061255131, positiony: \
       6.28751498859, velocityx: 0.467822918285, velocityy: \
       3.79795781204}; { positionx: 1.40529078049, positiony: \
       6.32549456671, velocityx: 0.455991107615, velocityy: \
       3.84294341802}; { positionx: 1.40985069157, positiony: \
       6.36392400089, velocityx: 0.444106953362, velocityy: \
       3.88755046539}; { positionx: 1.4142917611, positiony: \
       6.40279950555, velocityx: 0.432171384229, velocityy: \
       3.93177450865}; { positionx: 1.41861347494, positiony: \
       6.44211725063, velocityx: 0.420185340084, velocityy: \
       3.97561113836}; { positionx: 1.42281532835, positiony: \
       6.48187336202, velocityx: 0.408149771834, velocityy: \
       4.01905598161}; { positionx: 1.42689682606, positiony: \
       6.52206392183, velocityx: 0.396065641295, velocityy: \
       4.06210470244}; { positionx: 1.43085748248, positiony: \
       6.56268496886, velocityx: 0.38393392105, velocityy: \
       4.10475300231}; { positionx: 1.43469682169, positiony: \
       6.60373249888, velocityx: 0.371755594314, velocityy: \
       4.14699662053}; { positionx: 1.43841437763, positiony: \
       6.64520246509, velocityx: 0.359531654781, velocityy: \
       4.18883133469}; { positionx: 1.44200969418, positiony: \
       6.68709077843, velocityx: 0.347263106479, velocityy: \
       4.23025296114}; { positionx: 1.44548232524, positiony: \
       6.72939330804, velocityx: 0.334950963615, velocityy: \
       4.27125735537}; { positionx: 1.44883183488, positiony: \
       6.7721058816, velocityx: 0.322596250419, velocityy: \
       4.31184041251}; { positionx: 1.45205779738, positiony: \
       6.81522428572, velocityx: 0.310200000979, velocityy: \
       4.35199806771}; { positionx: 1.45515979739, positiony: \
       6.8587442664, velocityx: 0.297763259081, velocityy: \
       4.39172629658}; { positionx: 1.45813742998, positiony: \
       6.90266152937, velocityx: 0.285287078041, velocityy: \
       4.43102111564}; { positionx: 1.46099030076, positiony: \
       6.94697174052, velocityx: 0.272772520534, velocityy: \
       4.46987858275}; { positionx: 1.46371802597, positiony: \
       6.99167052635, velocityx: 0.26022065842, velocityy: \
       4.50829479747}; { positionx: 1.46632023255, positiony: \
       7.03675347432, velocityx: 0.247632572571, velocityy: \
       4.54626590157}; { positionx: 1.46879655828, positiony: \
       7.08221613334, velocityx: 0.235009352692, velocityy: \
       4.58378807937}; { positionx: 1.47114665181, positiony: \
       7.12805401413, velocityx: 0.22235209714, velocityy: \
       4.62085755818}; { positionx: 1.47337017278, positiony: \
       7.17426258971, velocityx: 0.209661912745, velocityy: \
       4.65747060873}; { positionx: 1.47546679191, positiony: \
       7.2208372958, velocityx: 0.196939914622, velocityy: \
       4.69362354553}; { positionx: 1.47743619105, positiony: \
       7.26777353126, velocityx: 0.184187225985, velocityy: \
       4.72931272732}; { positionx: 1.47927806331, positiony: \
       7.31506665853, velocityx: 0.171404977963, velocityy: \
       4.76453455741}; { positionx: 1.48099211309, positiony: \
       7.3627120041, velocityx: 0.15859430941, velocityy: \
       4.79928548415}; { positionx: 1.48257805618, positiony: \
       7.41070485895, velocityx: 0.145756366712, velocityy: \
       4.83356200125}; { positionx: 1.48403561985, positiony: \
       7.45904047896, velocityx: 0.132892303596, velocityy: \
       4.86736064818}; { positionx: 1.48536454289, positiony: \
       7.50771408544, velocityx: 0.120003280938, velocityy: \
       4.90067801059}; { positionx: 1.4865645757, positiony: \
       7.55672086555, velocityx: 0.107090466572, velocityy: \
       4.93351072064}; { positionx: 1.48763548036, positiony: \
       7.60605597275, velocityx: 0.0941550350886, velocityy: \
       4.9658554574}; { positionx: 1.48857703071, positiony: \
       7.65571452733, velocityx: 0.0811981676444, velocityy: \
       4.99770894721}; { positionx: 1.48938901239, positiony: \
       7.7056916168, velocityx: 0.0682210517643, velocityy: \
       5.02906796403}; { positionx: 1.49007122291, positiony: \
       7.75598229644, velocityx: 0.0552248811446, velocityy: \
       5.05992932982}; { positionx: 1.49062347172, positiony: \
       7.80658158974, velocityx: 0.0422108554557, velocityy: \
       5.09028991489}; { positionx: 1.49104558027, positiony: \
       7.85748448889, velocityx: 0.0291801801442, velocityy: \
       5.12014663822}; { positionx: 1.49133738208, positiony: \
       7.90868595527, velocityx: 0.0161340662352, velocityy: \
       5.14949646785}; { positionx: 1.49149872274, positiony: \
       7.96018091995, velocityx: 0.0030737301333, velocityy: \
       5.17833642117}; { positionx: 1.49152946004, positiony: \
       8.01196428416, velocityx: -0.00999960657508, velocityy: \
       5.20666356529}; { positionx: 1.49142946397, positiony: \
       8.06403091981, velocityx: -0.0230847173208, velocityy: \
       5.23447501735}; { positionx: 1.4911986168, positiony: \
       8.11637566999, velocityx: -0.0361803707502, velocityy: \
       5.26176794486}; { positionx: 1.49083681309, positiony: \
       8.16899334943, velocityx: -0.0492853309238, velocityy: \
       5.28853956599}; { positionx: 1.49034395978, positiony: \
       8.22187874509, velocityx: -0.0623983575146, velocityy: \
       5.3147871499}; { positionx: 1.48971997621, positiony: \
       8.27502661659, velocityx: -0.075518206006, velocityy: \
       5.34050801706}; { positionx: 1.48896479415, positiony: \
       8.32843169676, velocityx: -0.0886436278897, velocityy: \
       5.36569953951}; { positionx: 1.48807835787, positiony: \
       8.38208869216, velocityx: -0.101773370864, velocityy: \
       5.3903591412}; { positionx: 1.48706062416, positiony: \
       8.43599228357, velocityx: -0.114906179029, velocityy: \
       5.41448429825}; { positionx: 1.48591156237, positiony: \
       8.49013712655, velocityx: -0.128040793085, velocityy: \
       5.43807253926}]";
  ]

let suite =
  "test suite for final project"
  >::: List.flatten [ single_spring_tests; twod_spring_tests ]

let _ = run_test_tt_main suite