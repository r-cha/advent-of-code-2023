open Alcotest
open Aocamlib

let test_find_numbers () =
  let line = "..1..12..123.." in
  let expected = [ 9, 12; 5, 7; 2, 3 ] in
  let actual = Day_3.find_numbers line in
  check (list (pair int int)) "find_numbers" expected actual
;;

let test_process_seeds () =
  let seeds_line = "seeds: 79 14 55 13" in
  let expected = [ 79; 14; 55; 13 ] in
  let actual = Day_5.process_seeds seeds_line in
  check (list int) "parse_seeds" expected actual
;;

let test_build_map () =
  let map = "seed-to-soil map:\n50 98 2\n52 50 48" in
  let expected = 81 in
  let actual = Day_5.lookup 79 @@ Day_5.build_map map in
  check int "build_map" expected actual
;;

let tests =
  [ "find_numbers", `Quick, test_find_numbers
  ; "parse_seeds", `Quick, test_process_seeds
  ; "build_map", `Quick, test_build_map
  ]
;;

let () = Alcotest.run "Dummy" [ "AOCaml", tests ]
