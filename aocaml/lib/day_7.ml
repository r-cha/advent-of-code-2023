type hand =
  { cards : char list
  ; bid : int
  }

type hand_type =
  | FiveKind
  | FourKind
  | FullHouse
  | ThreeKind
  | TwoPair
  | OnePair
  | HighCard

let score_type = function
  | FiveKind -> 10
  | FourKind -> 9
  | FullHouse -> 8
  | ThreeKind -> 7
  | TwoPair -> 6
  | OnePair -> 5
  | HighCard -> 4
;;

let score_card = function
  | 'A' -> 14
  | 'K' -> 13
  | 'Q' -> 12
  | 'J' -> 11
  | 'T' -> 10
  | c -> int_of_char c - int_of_char '0'
;;

let count_unique_elements_naive list =
  (* Returns tuple of (element, count)*)
  let count_element e list = List.filter (fun x -> x = e) list |> List.length in
  List.sort_uniq Char.compare list |> List.map (fun e -> e, count_element e list)
;;

let score_hand (cards : char list) =
  let counts = count_unique_elements_naive cards in
  let sorted_counts = List.sort (fun (_, a) (_, b) -> b - a) counts in
  let kind =
    match List.length sorted_counts with
    | 1 -> FiveKind
    | 2 -> if snd (List.hd sorted_counts) = 4 then FourKind else FullHouse
    | 3 -> if snd (List.hd sorted_counts) = 3 then ThreeKind else TwoPair
    | 4 -> OnePair
    | 5 -> HighCard
    | _ -> failwith "Impossible hand type"
  in
  ()
;;

let run () =
  (* print_int (solve filename); *)
  print_newline ()
;;
