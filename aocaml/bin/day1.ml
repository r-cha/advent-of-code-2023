let is_num c =
    match c with
     '0'..'9' -> true
    | _ -> false;;

let rec calibrate = function
    | [] -> '0'
    | h :: t -> if is_num h then h else calibrate t;;

let calibration_value () =
    let line = read_line () in
    let l = List.init (String.length line) (String.get line) in
    let first = calibrate l in
    let last = calibrate (List.rev l) in
    String.make 1 first  ^ String.make 1 last;;

let rec accumulate_calibrations tot =
    try
        let v = int_of_string (calibration_value ()) in
        accumulate_calibrations v + tot
    with
        End_of_file -> tot;;

accumulate_calibrations 0
    |> string_of_int
    |> print_endline;;
