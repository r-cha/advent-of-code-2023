type num_indices =
  { pos : int
  ; current_num_start : int
  ; subs : (int * int) list
  }

let find_numbers line =
  let num_indices =
    String.fold_left
      (fun acc char ->
        match acc.current_num_start with
        | -1 ->
          if char >= '0' && char <= '9'
          then { acc with pos = acc.pos + 1; current_num_start = acc.pos }
          else { acc with pos = acc.pos + 1 }
        | _ ->
          if char >= '0' && char <= '9'
          then { acc with pos = acc.pos + 1 }
          else
            { pos = acc.pos + 1
            ; current_num_start = -1
            ; subs = (acc.current_num_start, acc.pos) :: acc.subs
            })
      { pos = 0; current_num_start = -1; subs = [] }
      line
  in
  if num_indices.current_num_start <> -1
  then (num_indices.current_num_start, String.length line) :: num_indices.subs
  else num_indices.subs
;;

module Chars = Set.Make (Char)

let symbols = Chars.of_list [ '@'; '#'; '$'; '%'; '^'; '&'; '*'; '/'; '+'; '='; '-' ]

type indices =
  { pos : int
  ; ids : int list
  }

let bounding_string line_no (start, end') all_text =
  let start_index = max 0 (start - 1) in
  let length = min (String.length all_text.(line_no) - 1) end' - start + 2 in
  let before =
    if line_no - 1 < 0 then "" else String.sub all_text.(line_no - 1) start_index length
  in
  let current = String.sub all_text.(line_no) start_index length in
  let after =
    if line_no + 1 > Array.length all_text - 1
    then ""
    else String.sub all_text.(line_no + 1) start_index length
  in
  before ^ current ^ after
;;

let process_number_range line_no (start, end') all_text =
  let check_string = bounding_string line_no (start, end') all_text in
  let _ = Printf.printf "CHECK: %s\tRANGE %i %i\n" check_string start end' in
  let approved =
    List.exists
      (fun char -> Chars.mem char symbols)
      (List.init (String.length check_string) (String.get check_string))
  in
  if approved
  then (
    let numstr = String.sub all_text.(line_no) start (end' - start) in
    let _ = Printf.printf "%s\n" numstr in
    try int_of_string numstr with
    | _ -> failwith @@ "Not a number " ^ numstr)
  else 0
;;

let determine_adjacency (numbers : (int * int) list list) all_text =
  (* Filter the indices based on their adjacency to a symbol *)
  let ids =
    List.fold_left
      (fun approved ids_list ->
        let line_no = approved.pos in
        let new_values =
          List.fold_left
            (fun acc range ->
              let v = process_number_range line_no range all_text in
              v :: acc)
            []
            ids_list
        in
        { pos = approved.pos + 1; ids = new_values @ approved.ids })
      { pos = 0; ids = [] }
      numbers
  in
  ids.ids
;;

let rec all_lines acc ic =
  try
    let line = input_line ic in
    all_lines (line :: acc) ic
  with
  | End_of_file -> List.rev acc
;;

let sum_part_numbers ic =
  let all_text = all_lines [] ic in
  let numbers =
    List.rev @@ List.fold_left (fun acc line -> find_numbers line :: acc) [] all_text
  in
  let text_array = Array.of_list all_text in
  let adjacency = determine_adjacency numbers text_array in
  List.fold_left ( + ) 0 adjacency
;;

let filename = "data/day_3.txt"

let solve filename =
  let ic = open_in filename in
  let result = sum_part_numbers ic in
  close_in ic;
  result
;;

let run () = solve filename |> print_int |> print_newline
