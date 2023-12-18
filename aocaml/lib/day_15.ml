let rec hash acc = function
  | [] -> acc
  | c :: cs -> hash ((acc + Char.code c) * 17 mod 256) cs
;;

let rec sum_hashes acc = function
  | [] -> acc
  | s :: ss -> sum_hashes (acc + hash 0 (s |> String.to_seq |> List.of_seq)) ss
;;

(* Part 1: 522547 *)

let hashmap = Hashtbl.create 256

let focusing_power box_number slot_number focal_length =
  (1 + box_number) * slot_number * focal_length
;;

type operation =
  | Assign
  | Remove
  | Noop

type lens =
  { label : string
  ; operation : operation
  ; focal_length : int
  }

let parse_lens cs =
  let rec aux label operation focal_length = function
    | [] ->
      { label = label |> List.rev |> List.to_seq |> String.of_seq
      ; operation
      ; focal_length
      }
    | ('a' .. 'z' as c) :: cs -> aux (c :: label) operation focal_length cs
    | '=' :: cs -> aux label Assign focal_length cs
    | '-' :: cs -> aux label Remove focal_length cs
    | ('0' .. '9' as c) :: cs -> aux label operation (int_of_string (String.make 1 c)) cs
    | _ :: _ -> failwith "Panic"
  in
  aux [] Noop (-1) cs
;;

let sum_focusing_powers box_number =
  let box = Hashtbl.find_opt hashmap box_number in
  match box with
  | Some lenses ->
    let _ = Printf.printf "\nBox %d | " box_number in
    List.fold_left
      ( + )
      0
      (List.mapi
         (fun i (label, fl) ->
           let _ = Printf.printf "%d: [%s %d] " i label fl in
           focusing_power box_number (i + 1) fl)
         (lenses |> List.rev))
  | None -> 0
;;

let replace_lens label length lenses =
  let rec replace_lens' acc = function
    | [] -> acc
    | (l, _) :: xs when l = label -> replace_lens' ((l, length) :: acc) xs
    | x :: xs -> replace_lens' (x :: acc) xs
  in
  List.rev @@ replace_lens' [] lenses
;;

let resolve_instruction box_number instruction =
  let box =
    match Hashtbl.find_opt hashmap box_number with
    | Some x -> x
    | None -> []
  in
  let new_lenses =
    match instruction.operation with
    | Assign ->
      let fl = List.assoc_opt instruction.label box in
      (match fl with
       | Some _ ->
         (* If a lens with this label exists, replace it *)
         replace_lens instruction.label instruction.focal_length box
       | None ->
         (* Otherwise, add it to the front of the box *)
         (instruction.label, instruction.focal_length) :: box)
    | Remove -> List.remove_assoc instruction.label box
    | Noop -> failwith "How did noop happen"
  in
  let _ = Hashtbl.replace hashmap box_number new_lenses in
  let _ = sum_focusing_powers box_number in
  ()
;;

let rec populate_boxes = function
  | [] -> ()
  | s :: xs ->
    let cs = s |> String.to_seq |> List.of_seq in
    let instruction = parse_lens cs in
    let box_number = instruction.label |> String.to_seq |> List.of_seq |> hash 0 in
    let _ = resolve_instruction box_number instruction in
    populate_boxes xs
;;

let sum_literally_all_focusing_powers () =
  let rec sum_literally_all_focusing_powers' acc i =
    if i = 256
    then acc
    else (
      let acc' = acc + sum_focusing_powers i in
      sum_literally_all_focusing_powers' acc' (i + 1))
  in
  sum_literally_all_focusing_powers' 0 0
;;

(* Part 2:
   3386258 TOO HIGH
   3288323 too high still
   63709 too low
   229271
*)

let read_whole_file filename =
  let ch = open_in_bin filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s
;;

let solve () =
  "data/day_15.txt" |> read_whole_file |> String.split_on_char ',' |> sum_hashes 0
;;

let solve_2 () =
  let instructions = "data/day_15.txt" |> read_whole_file |> String.split_on_char ',' in
  let _ = populate_boxes instructions in
  sum_literally_all_focusing_powers ()
;;

let run () =
  print_int (solve_2 ());
  print_newline ()
;;
