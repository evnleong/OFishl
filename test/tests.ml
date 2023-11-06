(** Test Plan: The first portion of our test suite is made of up OUnit tests, 
    *)

open OUnit2
(**open Final_project*)

let fish_tests = []
let tank_tests = []
let game_tests = []

let suite =
  "test suite" >::: List.flatten [ fish_tests; tank_tests; game_tests ]

let () = run_test_tt_main suite
