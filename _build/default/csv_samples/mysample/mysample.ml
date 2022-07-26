(** This file will output a csv in csv_samples/mysample/result*)

(** Note: If you want to play around with this file on utop, you must first have 
  this library installed (type in "opam install csv" into the terminal (NOT IN UTOP)).
  Then, in utop itself, start by typing in "#require "csv"" into utop before "#use"-ing it.*)

(** First, a function that takes in an integer and produces an int * int list, 
where the first int of each tuple is the index, and the second int is the tri number associated with that index.*)

let rec first_n_tri_nums_helper n cur_num acc = 
  match cur_num with
  | _ when cur_num = n + 1 -> []
  | _ -> (cur_num , cur_num + acc) :: first_n_tri_nums_helper (n) (cur_num + 1) (cur_num + acc)

(** Requires: n > 0*)
let first_n_tri_nums n = first_n_tri_nums_helper n 1 0

let rec tuple_lst_to_embedded_string lst = 
  "\ " ^
  match lst with
  | [] -> ""
  | (num, tri_num) :: t -> ("\"" ^ (string_of_int num) ^ "\",\"" ^ (string_of_int tri_num) ^ "\"\n") ^ (tuple_lst_to_embedded_string t)


(** This function takes in an integer, and outputs, as a csv file, the first n triangular numbers.*)
let first_n_tri_nums_to_escv n = 
  let ecsv = Csv.input_all (Csv.of_string (tuple_lst_to_embedded_string (first_n_tri_nums n))) in 
  Csv.print_readable ecsv;
  let fname = Filename.concat ("result") "mysampleresult.csv" in
  Csv.save fname ecsv;

