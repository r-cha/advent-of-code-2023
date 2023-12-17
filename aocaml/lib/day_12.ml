type record =
  { conditions : string
  ; groups : int list
  }

(*
   In this example, the number of possible arrangements for each row is:

   ???.### 1,1,3 - 1 arrangement
   .??..??...?##. 1,1,3 - 4 arrangements
   ?#?#?#?#?#?#?#? 1,3,1,6 - 1 arrangement
   ????.#...#... 4,1,1 - 1 arrangement
   ????.######..#####. 1,6,5 - 4 arrangements
   ?###???????? 3,2,1 - 10 arrangements
*)

type string_accumulator = string list * char list

let extract_groups record =
  let chars = List.of_seq @@ String.to_seq record.conditions in
  let folder acc c =
    match c with
    | '?' | '#' -> fst acc, c :: snd acc
    | _ ->
      if snd acc = []
      then fst acc, snd acc
      else String.of_seq (List.to_seq (List.rev (snd acc))) :: fst acc, []
  in
  let groups_acc = List.fold_left folder ([], []) chars in
  let groups =
    if snd groups_acc = []
    then fst groups_acc
    else String.of_seq (List.to_seq (List.rev (snd groups_acc))) :: fst groups_acc
  in
  groups
;;

let score_group group =
  let max_score = String.length group in
  let min_score = () (*Shortest contiguous # substring *) in
  ()
;;

let find_arrangements record =
  (* The rest of the owl *)
  List.hd record.groups
;;

let parse_line line =
  match String.split_on_char ' ' line with
  | [ conditions; groups ] ->
    { conditions; groups = String.split_on_char ',' groups |> List.map int_of_string }
  | _ -> failwith "Invalid input"
;;

let read_whole_file filename =
  let ch = open_in_bin filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s
;;

let solve filename =
  read_whole_file filename
  |> String.split_on_char '\n'
  |> List.map parse_line
  |> List.map find_arrangements
  |> List.fold_left ( + ) 0
;;

let filename = "data/day_12.txt"

let run () =
  print_int (solve filename);
  print_newline ()
;;
