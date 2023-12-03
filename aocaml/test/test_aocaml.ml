open Alcotest
open Aocamlib

let test_find_symbols () =
  let line = "abc@def#ghi" in
  let expected = [ 7; 3 ] in
  let actual = Day_3.find_symbols line in
  check (list int) "find_symbols" expected actual
;;

let test_find_numbers () =
  let line = "..1..12..123.." in
  let expected = [ 9, 12; 5, 7; 2, 3 ] in
  let actual = Day_3.find_numbers line in
  check (list (pair int int)) "find_numbers" expected actual
;;

let tests =
  [ "find_symbols", `Quick, test_find_symbols; "find_numbers", `Quick, test_find_numbers ]
;;

let () = Alcotest.run "Dummy" [ "AOCaml", tests ]
