let rec hash acc = function
  | [] -> acc
  | c :: cs -> hash ((acc + Char.code c) * 17 mod 256) cs
;;

let rec sum_hashes acc = function
  | [] -> acc
  | s :: ss -> sum_hashes (acc + hash 0 (s |> String.to_seq |> List.of_seq)) ss
;;

let read_whole_file filename =
  let ch = open_in_bin filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s
;;

(* Part 1: 522547 *)

let solve () =
  "data/day_15.txt" |> read_whole_file |> String.split_on_char ',' |> sum_hashes 0
;;

let run () =
  print_int (solve ());
  print_newline ()
;;
