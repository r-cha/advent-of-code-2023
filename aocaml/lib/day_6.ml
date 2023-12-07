(*
   Races last Time milliseconds and the record distance is distance.
   Holding the button for X milliseconds makes the boat travel X milliseconds per second upon release.

   (7, 9) -> [2,5]
   (15, 40) -> [4, 11]
   (30, 200) -> [11, 19]
*)

let test = "Time:      7  15   30\nDistance:  9  40  200"

type race =
  { time : int
  ; distance : int
  }

let parse input =
  let fix_line line =
    List.nth (String.split_on_char ':' line) 1
    |> String.trim
    |> String.split_on_char ' '
    |> List.filter (fun s -> String.length s > 0)
    |> List.map int_of_string
  in
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
      if winning r.distance @@ distance ~held ~allowed:r.time
      then (* let _ = Printf.printf "%d " held in *)
        count + 1
      else count)
    0
    (range (r.time - 1))
;;

let margin prod_acc (r : race) =
  let _ = Printf.printf "RACE %d %d  |  " r.time r.distance in
  let winning_ways = ways_to_win r in
  let _ = print_endline "" in
  prod_acc * winning_ways
;;

let solve input = input |> parse |> List.fold_left margin 1
let filename = "data/day_6.txt"

let solve filename =
  let read_whole_file filename =
    (* open_in_bin works correctly on Unix and Windows *)
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
