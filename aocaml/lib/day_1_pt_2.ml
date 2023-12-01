let rec parse_numbers clist =
  match clist with
  | [] -> []
  | '0'..'9' :: t -> int_of_string (String.make 1 (List.hd clist)) :: parse_numbers t
  | 'o' :: 'n' :: 'e' :: t -> 1 :: parse_numbers t
  | 't' :: 'w' :: 'o' :: t -> 2 :: parse_numbers t
  | 't' :: 'h' :: 'r' :: 'e' :: 'e' :: t -> 3 :: parse_numbers t
  | 'f' :: 'o' :: 'u' :: 'r' :: t -> 4 :: parse_numbers t
  | 'f' :: 'i' :: 'v' :: 'e' :: t -> 5 :: parse_numbers t
  | 's' :: 'i' :: 'x' :: t -> 6 :: parse_numbers t
  | 's' :: 'e' :: 'v' :: 'e' :: 'n' :: t -> 7 :: parse_numbers t
  | 'e' :: 'i' :: 'g' :: 'h' :: 't' :: t -> 8 :: parse_numbers t
  | 'n' :: 'i' :: 'n' :: 'e' :: t -> 9 :: parse_numbers t
  | _ :: t -> parse_numbers t;;

let calibration_value_2 () =
  let line = read_line () in
  let numbers = List.init (String.length line) (String.get line) |> parse_numbers in
  (List.hd numbers * 10) + List.hd (List.rev numbers);;

let rec accumulate_calibrations_2 tot =
  try
      let v = calibration_value_2 () in
      accumulate_calibrations_2 (v + tot)
  with
      End_of_file -> tot;;

let solve = accumulate_calibrations_2 0;;
