type game =
  { id : int (* This field is polymorphic for power in part 2 *)
  ; red : int
  ; green : int
  ; blue : int
  }

let empty_game = { id = 0; red = 0; green = 0; blue = 0 }
let elf_game = { id = 0; red = 12; green = 13; blue = 14 }

let is_possible g =
  (* A game is possible if no red, green, or blue is greater than elf_game red, green, or blue*)
  g.red <= elf_game.red && g.green <= elf_game.green && g.blue <= elf_game.blue
;;

let process_color (c : string) =
  let split_data = String.split_on_char ' ' (String.trim c) in
  match split_data with
  | [ amount; color ] -> color, int_of_string amount
  | _ -> failwith ("Invalid color string>" ^ c)
;;

let rec create_subgame kvs game =
  match kvs with
  | [] -> game
  | (color, amount) :: t ->
    (match color with
     | "red" -> create_subgame t { game with red = amount }
     | "green" -> create_subgame t { game with green = amount }
     | "blue" -> create_subgame t { game with blue = amount }
     | _ -> failwith ("Invalid color " ^ color))
;;

let process_revelation (r : string) =
  let color_kvs = String.split_on_char ',' r in
  let kvs = List.map process_color color_kvs in
  create_subgame kvs empty_game
;;

let update_game game revelation =
  { game with
    red = max game.red revelation.red
  ; green = max game.green revelation.green
  ; blue = max game.blue revelation.blue
  }
;;

let rec process_revelations revelations game =
  (* This is where we accumulate the max for each color across all revelations *)
  match revelations with
  | [] -> game
  | h :: t -> process_revelation h |> update_game game |> process_revelations t
;;

let process_game (line : string) id_fun =
  let split_data = String.split_on_char ':' line in
  let metastring = List.hd split_data in
  let gamestring =
    match split_data with
    | [] -> ""
    | _ :: t -> String.concat "" t
  in
  let id = int_of_string (String.sub metastring 5 (String.length metastring - 5)) in
  let revelations = String.split_on_char ';' gamestring in
  let processed = process_revelations revelations { empty_game with id } in
  { processed with id = id_fun processed }
;;

let rec sum_ids tot ic checker id_fun =
  try
    let line = input_line ic in
    let game = process_game line id_fun in
    let new_tot = if checker game then tot + game.id else tot in
    sum_ids new_tot ic checker id_fun
  with
  | End_of_file -> tot
;;

let power g =
  (* For part 2 *)
  g.red * g.green * g.blue
;;

let filename = "data/day_2.txt"

let solve filename pt =
  let checker = if pt = 1 then is_possible else fun _ -> true in
  let id_fun = if pt = 1 then fun g -> g.id else power in
  let ic = open_in filename in
  let result = sum_ids 0 ic checker id_fun in
  close_in ic;
  result
;;

let compare () =
  print_int (solve filename 1);
  print_newline ();
  print_int (solve filename 2);
  print_newline ()
;;
