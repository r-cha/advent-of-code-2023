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
  let map = [ "seed-to-soil map:"; "50 98 2"; "52 50 48" ] in
  let test_data = [ 79, 81; 14, 14; 55, 57; 13, 13 ] in
  let made_map = Day_5.build_map map in
  for i = 0 to List.length test_data - 1 do
    let seed, expected = List.nth test_data i in
    let actual = Day_5.lookup seed made_map in
    check int "build_map" expected actual
  done
;;

let tests =
  [ "find_numbers", `Quick, test_find_numbers
  ; "parse_seeds", `Quick, test_process_seeds
  ; "build_map", `Quick, test_build_map
  ]
;;

let () = Alcotest.run "Dummy" [ "AOCaml", tests ]
