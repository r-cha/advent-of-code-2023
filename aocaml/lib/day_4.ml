module Ints = Set.Make (Int)

type game =
  { winning : Ints.t
  ; present : Ints.t
  }

let empty_game = { winning = Ints.empty; present = Ints.empty }

let process_nums (c : string) =
  (* Split a string of nums and make a set *)
  let split_data = String.split_on_char ' ' (String.trim c) in
  List.fold_left
    (fun acc numstr ->
      match numstr with
      | "" -> acc
      | _ ->
        let num = int_of_string numstr in
        Ints.add num acc)
    Ints.empty
    split_data
;;

let process_game (line : string) =
  let split_data = String.split_on_char ':' line in
  let game =
    match split_data with
    | [] -> failwith "No data"
    | _ :: t ->
      (match t with
       | [] -> failwith "No data"
       | [ x ] -> x
       | _ -> failwith "Too much data")
  in
  let winning, present =
    match game with
    | "" -> failwith "No data"
    | _ ->
      (match String.split_on_char '|' game with
       | [ x; y ] -> process_nums x, process_nums y
       | _ -> failwith "Too much data")
  in
  { winning; present }
;;

let score_game (g : game) =
  let overlap = Ints.inter g.winning g.present in
  let num_matched = Ints.cardinal overlap in
  (* 0, 1, 2, 4, 8, 16 depending on num_matched *)
  let score = 1 lsl (num_matched - 1) in
  let _ = print_int score |> print_newline in
  score
;;

let rec sum_ids tot ic =
  try
    let line = input_line ic in
    let score = process_game line |> score_game in
    sum_ids (tot + score) ic
  with
  | End_of_file -> tot
;;

let filename = "data/day_4.txt"

let solve filename =
  let ic = open_in filename in
  let result = sum_ids 0 ic in
  close_in ic;
  result
;;

let run () =
  print_int (solve filename);
  print_newline ()
;;
