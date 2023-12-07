type race =
  { time : int
  ; distance : int
  }

let fix_line_1 line =
  List.nth (String.split_on_char ':' line) 1
  |> String.trim
  |> String.split_on_char ' '
  |> List.filter (fun s -> String.length s > 0)
  |> List.map int_of_string
;;

let fix_line_2 line =
  [ line
    |> String.fold_left
         (fun acc c ->
           match c with
           | '0' .. '9' -> c :: acc
           | _ -> acc)
         []
    |> List.rev
    |> List.to_seq
    |> String.of_seq
    |> int_of_string
  ]
;;

let parse input =
  let fix_line = fix_line_2 in
  (* Just hotswap fix_line for running different parts*)
  let lines = String.split_on_char '\n' input |> List.map fix_line in
  let times = List.nth lines 0 in
  let distances = List.nth lines 1 in
  List.map2 (fun time distance -> { time; distance }) times distances
;;

let range n = List.init n succ
let distance ~held ~allowed = (allowed - held) * held
let winning record achieved = achieved > record

let ways_to_win (r : race) =
  (* The most naive possible approach *)
  List.fold_left
    (fun count held ->
      if winning r.distance @@ distance ~held ~allowed:r.time then count + 1 else count)
    0
    (range (r.time - 1))
;;

let margin prod_acc (r : race) =
  let winning_ways = ways_to_win r in
  let _ = print_endline "" in
  prod_acc * winning_ways
;;

let solve input = input |> parse |> List.fold_left margin 1
let filename = "data/day_6.txt"

let solve filename =
  let read_whole_file filename =
    let ch = open_in_bin filename in
    let s = really_input_string ch (in_channel_length ch) in
    close_in ch;
    s
  in
  let input = read_whole_file filename in
  solve input
;;

let run () =
  print_int (solve filename);
  print_newline ()
;;
