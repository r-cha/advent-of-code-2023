let int_of_charlist cl =
  let s = cl |> List.rev |> List.to_seq |> String.of_seq |> String.trim in
  int_of_string s
;;

let drop_first = function
  (* Drop the first element of a list *)
  | [] | [ _ ] -> failwith "Can't drop this"
  | _ :: rest -> rest
;;

type seed_acc =
  { current : char list
  ; all : int list
  }

let process_seeds line =
  let folder acc c =
    (* Process the seed line char-by-char to build the seed numbers *)
    match c with
    | ' ' ->
      (match acc.current with
       | [] -> acc
       | _ -> { current = []; all = int_of_charlist acc.current :: acc.all })
    | '0' .. '9' -> { acc with current = c :: acc.current }
    | _ -> acc
  in
  let processed = String.fold_left folder { current = []; all = [] } line in
  let processed =
    match processed.current with
    | [] -> processed
    | _ -> { current = []; all = int_of_charlist processed.current :: processed.all }
  in
  List.rev processed.all
;;

type map_row =
  { destination_start : int
  ; source_start : int
  ; range : int
  }

type map = map_row list

let build_map l =
  let rows = drop_first l in
  let folder acc row =
    (* Build a list of map_rows one row at a time *)
    let parts = List.map int_of_string @@ String.split_on_char ' ' row in
    match parts with
    | [ d; s; r ] -> { destination_start = d; source_start = s; range = r } :: acc
    | _ -> failwith "invalid row"
  in
  List.rev @@ List.fold_left folder [] rows
;;

let rec lookup source ranges =
  (* Given a source number, find its first destination number from the given map.
     Defaults to source when not found. *)
  match ranges with
  | [] -> source
  | row :: tail ->
    let dest =
      let diff = source - row.source_start in
      if diff < row.range && diff >= 0
      then row.destination_start + diff
      else lookup source tail
    in
    dest
;;

let rec follow_maps maps source =
  (* Punch through the list of maps *)
  match maps with
  | [] -> source
  | h :: t ->
    let dest = lookup source h in
    let _ = Printf.printf "%d -> %d\n" source dest in
    follow_maps t dest
;;

let split_maps l =
  (* Split list of strings by double-newlines *)
  fst
  @@ List.fold_left
       (fun (acc, current) line ->
         match line with
         | "" ->
           (match current with
            | [] -> acc, current
            | _ -> List.rev current :: acc, [])
         | _ -> acc, line :: current)
       ([], [])
       l
  |> List.rev
;;

let do_the_thing input =
  let lines = input |> String.split_on_char '\n' in
  let seeds = lines |> List.hd |> process_seeds in
  let maps = lines |> drop_first |> split_maps |> List.map build_map in
  let results = List.map (follow_maps maps) seeds in
  let _ = List.iter (Printf.printf "%d\t") results in
  let _ = print_newline () in
  (* follow_maps maps (List.hd seeds) *)
  List.fold_left min (List.hd results) results
;;

let filename = "data/day_5.txt"
(*
   156940322 too high
   156940322 still just as high
*)

let solve filename =
  let read_whole_file filename =
    let ch = open_in_bin filename in
    let s = really_input_string ch (in_channel_length ch) in
    close_in ch;
    s
  in
  let input = read_whole_file filename in
  do_the_thing input
;;

let run () =
  print_int (solve filename);
  print_newline ()
;;
