let rec read_elf sum =
  let line = read_line () in
  if line = "" then sum else read_elf (sum + int_of_string line)
;;

let rec read_elves max =
  try
    let sum = read_elf 0 in
    if sum > max then read_elves sum else read_elves max
  with
  | End_of_file -> max
;;

print_endline (string_of_int (read_elves 0))
