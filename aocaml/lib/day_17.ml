(* type move =
  | Left
  | Right
  | Up
  | Down

type state =
  { last_3_moves : move list
  ; cumulative_heat_loss : int
  } *)

(* Trying something new... *)

type node = int * int (* (x, y) *)

module NodeMap = Map.Make (struct
    type t = node

    let compare = compare
  end)

type graph' = (node * int) list NodeMap.t
type priority_queue = (int * node) list

let rec insert queue priority value =
  match queue with
  | [] -> [ priority, value ]
  | (p, v) :: tail ->
    if priority < p
    then (priority, value) :: queue
    else (p, v) :: insert tail priority value
;;

let extract_min = function
  | [] -> failwith "Priority queue is empty"
  | min :: rest -> min, rest
;;

let dijkstra graph start_node =
  let rec search queue distances =
    if queue = []
    then distances
    else (
      let (dist, current_node), queue = extract_min queue in
      match NodeMap.find_opt current_node distances with
      | Some known_dist when known_dist <= dist -> search queue distances
      | _ ->
        let distances = NodeMap.add current_node dist distances in
        let edges =
          match NodeMap.find_opt current_node graph with
          | None -> []
          | Some edges -> edges
        in
        let queue =
          List.fold_left
            (fun queue (neighbor, weight) ->
              match NodeMap.find_opt neighbor distances with
              | Some known_dist when known_dist <= dist + weight -> queue
              | _ -> insert queue (dist + weight) neighbor)
            queue
            edges
        in
        search queue distances)
  in
  search [ 0, start_node ] NodeMap.empty
;;

let build_factory_map text =
  (* TODO: Actually parse file into physical map *)
  let _ = text in
  NodeMap.empty
  |> NodeMap.add (0, 0) [ (1, 0), 4; (0, 1), 2 ]
  |> NodeMap.add (1, 0) [ (1, 1), 5 ]
  |> NodeMap.add (0, 1) [ (1, 1), 3 ]
  |> NodeMap.add (1, 1) []
;;

(*
   NOTES:
   I want to explore the state graph, not the physical graph.
   Each node should know
   - its location,
   - its momentum, and
   - its cumulative heat loss.

   Time to pull out Russell and Norvig... and maybe check out my old pacman assignments.
*)

let read_whole_file filename =
  let ch = open_in_bin filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s
;;

let solve () =
  let factory_map = "data/day_17_test.txt" |> read_whole_file |> build_factory_map in
  let distances = dijkstra factory_map (0, 0) in
  distances |> NodeMap.find (1, 1)
;;

let run () =
  print_int (solve ());
  print_newline ()
;;
