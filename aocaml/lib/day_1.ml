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
    let first = String.make 1 (calibrate l) in
    let last = String.make 1 (calibrate (List.rev l)) in
    first ^ last;;

let rec accumulate_calibrations tot =
    try
        let v = int_of_string (calibration_value ()) in
        accumulate_calibrations (v + tot)
    with
        End_of_file -> tot;;

let solve = accumulate_calibrations 0;;
