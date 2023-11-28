(** Test Plan: The first portion of our test suite is made of up OUnit tests, 
    *)

open OUnit2
open Final_project
open Game

let goldfish = make_fish Goldfish Pellet 1.4 0.6
let remora = make_fish Remora Pellet 1.2 0.8
let game = Game.start_game 10
let _ = set_game game
let game2 = Game.start_game 10
let _ = set_game game2
let _ = buy_fish_game game2 Shark 2
let _ = health_tank_species (get_tank game2) Shark (-80.)
let _ = add_fish remora 10
let _ = age_fish remora
let game3 = Game.start_game 10

let _ =
  buy_fish_game game3 Anemone 7;
  health_tank_species (get_tank game3) Anemone (-50.);
  feed_fish_game game3 Anemone (7 * 50)

let fish_tests =
  [
    ("make empty goldfish" >:: fun _ -> assert_equal 0 (get_age goldfish));
    ("price of 1 goldfish" >:: fun _ -> assert_equal 2. (price_fish Goldfish 1));
    ("price of 10 sharks" >:: fun _ -> assert_equal 200. (price_fish Shark 10));
    ("Extinct Shark" >:: fun _ -> assert_equal true (species_extinct game Shark));
    ( "Not Extinct Shark" >:: fun _ ->
      assert_equal false (species_extinct game2 Shark) );
    ("Start w/ 100" >:: fun _ -> assert_equal 100. (get_playermoney game));
    ("Cost 40." >:: fun _ -> assert_equal 60. (get_playermoney game2));
    ( "Healthy fish species" >:: fun _ ->
      assert_equal 100. (get_health game Goldfish) );
    ("Sick sharks" >:: fun _ -> assert_equal 20. (get_health game2 Shark));
    ("add 10 remora" >:: fun _ -> assert_equal 10 (get_num remora));
    ("age 10 remora" >:: fun _ -> assert_equal 10 (get_age remora));
    ( "Feed hungry anemone" >:: fun _ ->
      assert_equal 100. (get_health game3 Anemone) );
  ]

let tank_tests = []
let game_tests = []

let suite =
  "test suite" >::: List.flatten [ fish_tests; tank_tests; game_tests ]

let () = run_test_tt_main suite
