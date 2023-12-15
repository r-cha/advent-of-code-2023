type hand_type =
  | FiveKind
  | FourKind
  | FullHouse
  | ThreeKind
  | TwoPair
  | OnePair
  | HighCard

type hand =
  { cards : char list
  ; bid : int
  ; kind : hand_type
  }

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

let determine_kind (cards : char list) =
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
  kind
;;

let break_tie (cards1 : char list) (cards2 : char list) =
  let rec aux (cards : (char * char) list) =
    match cards with
    | [] -> 0
    | (c1, c2) :: t ->
      let score1 = score_card c1 in
      let score2 = score_card c2 in
      if score1 > score2 then 1 else if score1 < score2 then -1 else aux t
  in
  aux (List.combine cards1 cards2)
;;

let compare_hands (hand1 : hand) (hand2 : hand) =
  if score_type hand1.kind > score_type hand2.kind
  then 1
  else if score_type hand1.kind < score_type hand2.kind
  then -1
  else break_tie hand1.cards hand2.cards
;;

let calculate_winnings (h : hand) (rank : int) = rank * h.bid

let calculate_total_winnings (hands : hand list) =
  let sorted_hands = List.sort compare_hands hands in
  let rec aux (hands : hand list) (rank : int) =
    match hands with
    | [] -> 0
    | h :: t -> calculate_winnings h rank + aux t (rank + 1)
  in
  aux sorted_hands 1
;;

let parse_hand line =
  match String.split_on_char ' ' line with
  | [] -> failwith "Empty line"
  | [ cardstring; bidstring ] ->
    let cards = String.to_seq cardstring |> List.of_seq in
    let bid = int_of_string bidstring in
    { cards; bid; kind = HighCard }
  | _ -> failwith "Invalid line"
;;

let rec parse_hands lines =
  match lines with
  | [] -> []
  | h :: t -> parse_hand h :: parse_hands t
;;

let read_whole_file filename =
  let ch = open_in_bin filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s
;;

let solve filename =
  let text = read_whole_file filename in
  let hands = parse_hands (String.split_on_char '\n' text) in
  let scored_hands = List.map (fun h -> { h with kind = determine_kind h.cards }) hands in
  calculate_total_winnings scored_hands
;;

let filename = "data/day_7.txt"

(* Part 1: 250254244 *)

let run () =
  print_int (solve filename);
  print_newline ()
;;
