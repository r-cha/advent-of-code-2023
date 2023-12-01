let rec parse_numbers_1 clist =
    match clist with
    | [] -> -1
    | '0'..'9' :: _ -> int_of_string (String.make 1 (List.hd clist))
    | _ :: t -> parse_numbers_1 t;;

let rec parse_numbers_2 clist =
    match clist with
    | [] -> -1 (* idk, shouldn't happen*)
    | '0'..'9' :: _ -> int_of_string (String.make 1 (List.hd clist))
    | 'o' :: 'n' :: 'e' :: _ -> 1
    | 't' :: 'w' :: 'o' :: _ -> 2
    | 't' :: 'h' :: 'r' :: 'e' :: 'e' :: _ -> 3
    | 'f' :: 'o' :: 'u' :: 'r' :: _ -> 4
    | 'f' :: 'i' :: 'v' :: 'e' :: _ -> 5
    | 's' :: 'i' :: 'x' :: _ -> 6
    | 's' :: 'e' :: 'v' :: 'e' :: 'n' :: _ -> 7
    | 'e' :: 'i' :: 'g' :: 'h' :: 't' :: _ -> 8
    | 'n' :: 'i' :: 'n' :: 'e' :: _ -> 9
    | _ :: t -> parse_numbers_2 t;;

let rec reverse_parse_numbers_2 clist =
    match clist with
    | [] -> -1 (* idk, shouldn't happen*)
    | '0'..'9' :: _ -> int_of_string (String.make 1 (List.hd clist))
    | 'e' :: 'n' :: 'o' :: _ -> 1
    | 'o' :: 'w' :: 't' :: _ -> 2
    | 'e' :: 'e' :: 'r' :: 'h' :: 't' :: _ -> 3
    | 'r' :: 'u' :: 'o' :: 'f' :: _ -> 4
    | 'e' :: 'v' :: 'i' :: 'f' :: _ -> 5
    | 'x' :: 'i' :: 's' :: _ -> 6
    | 'n' :: 'e' :: 'v' :: 'e' :: 's' :: _ -> 7
    | 't' :: 'h' :: 'g' :: 'i' :: 'e' :: _ -> 8
    | 'e' :: 'n' :: 'i' :: 'n' :: _ -> 9
    | _ :: t -> reverse_parse_numbers_2 t;;

type direction = Forward | Backward;;
let parser_1 clist dir = let _ = dir in parse_numbers_1 clist;;
let parser_2 clist dir =
    match dir with
    | Forward -> parse_numbers_2 clist
    | Backward -> reverse_parse_numbers_2 clist

let calibration_value line parser =
    let numlist = List.init (String.length line) (String.get line) in
    let first = parser numlist Forward in
    let last = parser (List.rev numlist) Backward in
    (first * 10) + last;;

let rec accumulate_calibrations tot ic parser =
    try
        let line = input_line ic in
        let v = calibration_value line parser in
        accumulate_calibrations (v + tot) ic parser
    with
        End_of_file -> tot;;

let solve filename pt =
    let parser = if pt = 1 then parser_1 else parser_2 in
    let ic = open_in filename in
    let result = accumulate_calibrations 0 ic parser in
    close_in ic;
    result;;

let filename = "data/day_1.txt";;

let compare () =
    print_int (solve filename 1);
    print_newline ();
    print_int (solve filename 2);
    print_newline ();;
