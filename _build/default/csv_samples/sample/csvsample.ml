(** (jdc394) THIS CODE IS NOT OURS. Courtesy of https://github.com/Chris00/ocaml-csv. This file is merely
  here as a sample to help us structure our csv files.*)

(* See also 'test.ml' for examples, and 'csv.mli' for documentation. *)

open Printf

(* In order to parse a string into a csv, it must take up this form. Compare this to "example.csv" to see how
this form translates to a csv file.*)
let embedded_csv = "\
\"Banner cickins\"
\"Clickin\",\"Number\",\"Percentage\",
\"brand.adwords\",\"4,878\",\"14.4\"
\"vacation.advert2.adwords\",\"4,454\",\"13.1\"
\"affiliates.generic.tc1\",\"1,608\",\"4.7\"
\"brand.overture\",\"1,576\",\"4.6\"
\"vacation.cheap.adwords\",\"1,515\",\"4.5\"
\"affiliates.generic.vacation.biggestchoice\",\"1,072\",\"3.2\"
\"breaks.no-destination.adwords\",\"1,015\",\"3.0\"
\"fly.no-destination.flightshome.adwords\",\"833\",\"2.5\"
\"exchange.adwords\",\"728\",\"2.1\"
\"holidays.cyprus.cheap\",\"574\",\"1.7\"
\"travel.adwords\",\"416\",\"1.2\"
\"affiliates.vacation.generic.onlinediscount.200\",\"406\",\"1.2\"
\"promo.home.topX.ACE.189\",\"373\",\"1.1\"
\"homepage.hp_tx1b_20050126\",\"369\",\"1.1\"
\"travel.agents.adwords\",\"358\",\"1.1\"
\"promo.home.topX.SSH.366\",\"310\",\"0.9\""

(* For your purposes, this part of the code does not matter. All it does is
load in info from an existing csv. Since we are just writing to a csv file, this part should not matter.*)
let csvs =
  List.map (fun name -> name, Csv.load name)
           [ "examples/example1.csv"; "examples/example2.csv" ]


let () =
  (* This is printing out our embedded_csv (i.e. the csv in string form up above)
into a form where it can be printed out to the console.*)
  let ecsv = Csv.input_all(Csv.of_string embedded_csv) in
    printf "---Embedded CSV---------------------------------\n" ;
    Csv.print_readable ecsv;

    (* Can ignore this section of code - again, involves already existing csvs.*)
  List.iter (
    fun (name, csv) ->
      printf "---%s----------------------------------------\n" name;
      Csv.print_readable csv
  ) csvs;
  printf "Compare (Embedded CSV) example1.csv = %i\n"
         (Csv.compare ecsv (snd(List.hd csvs)))

(* Save it to a file *)
let () =
(* converts string version of csv into a form where... *)
  let ecsv = Csv.input_all(Csv.of_string embedded_csv) in
(* ... we can save at this location in the directory.*)
  let fname = Filename.concat ("examples") "example.csv" in
  Csv.save fname ecsv;
  printf "Saved CSV to file %S.\n" fname