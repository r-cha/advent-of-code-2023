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
  score
;;

let score_game_2 (g : game) =
  let overlap = Ints.inter g.winning g.present in
  let num_matched = Ints.cardinal overlap in
  num_matched
;;

(*
   Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53  | SCORE:4; MULTIPLICITY:1;
   Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19  | SCORE:2; MULTIPLICITY:1+1;
   Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1  | SCORE:2; MULTIPICITY:1+1+2;
   Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83  | SCORE:1; MULTIPLICITY:1+1+2+8;
   Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36  | SCORE:0; MULTIPLICITY:1+1+...
   Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11  | SCORE:0; etc
*)

let count_cards (scores : int list) =
  let counts = Array.make (List.length scores) 1 in
  (* For each new score, pop the multiplier m off the list of counts, then add m to the next x counts *)
  let _ =
    (* Please forgive my imperative code *)
    for i = 0 to List.length scores - 1 do
      (* For each score in the pile *)
      let x = List.nth scores i in
      (* The number of cards to multiply *)
      let m = counts.(i) in
      (* The multiplier *)
      let _ = Printf.printf "Card %d: SCORE%d MULT%d\n" i x m in
      for j = i + 1 to i + x do
        counts.(j) <- counts.(j) + m
      done
    done
  in
  Array.fold_left ( + ) 0 counts
;;

let sum_ids ic =
  let rec sum_ids' tot ic =
    try
      let line = input_line ic in
      let score = process_game line |> score_game in
      sum_ids' (tot + score) ic
    with
    | End_of_file -> tot
  in
  sum_ids' 0 ic
;;

let sum_ids_2 ic =
  let rec sum_ids_2' acc ic =
    try
      let line = input_line ic in
      let score = process_game line |> score_game_2 in
      sum_ids_2' (score :: acc) ic
    with
    | End_of_file -> acc
  in
  let scores = sum_ids_2' [] ic in
  count_cards scores
;;

let filename = "data/day_4.txt"

let solve filename pt =
  let solver = if pt = 1 then sum_ids else sum_ids_2 in
  let ic = open_in filename in
  let result = solver ic in
  close_in ic;
  result
;;

let run () =
  print_int (solve filename 1);
  print_newline ();
  print_int (solve filename 2);
  print_newline ()
;;
