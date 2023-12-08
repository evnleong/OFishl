(** Test Plan: Our test suite is made up of OUnit tests, designed using black 
    box based on our function specifications and glass box testing to test 
    exceptions. The majority of our functions rely on mutability and therefore 
    only return units. To test these functions, we make a couple of different 
    game states relying on different features of the game. In this test file we 
    include tests for our Game module. The Userinput module was manually tested 
    in the terminal along with main.ml.
    This test plan demonstrates correctness our system by covering base cases 
    and typical game behavior while also exploring corner cases and exceptions. 
    *)

open OUnit2
open Final_project
open Game

let goldfish = make_fish Goldfish Pellet 1.4 0.6
let remora = make_fish Remora Pellet 1. 0.8
let game = start_game 10

let _ =
  set_game game;
  age_tank (get_tank game)

let game2 = start_game 5

let _ =
  set_game game2;
  buy_fish_game game2 Shark 2;
  health_tank_species (get_tank game2) Shark (-80.);
  add_fish remora 10;
  age_fish remora

let game3 = start_game 10

let _ =
  set_game game3;
  buy_fish_game game3 Clownfish 7;
  buy_fish_game game3 Anemone 7;
  health_tank_species (get_tank game3) Anemone (-50.);
  feed_fish_game game3 Anemone (7 * 50)

let game4 = start_game 3

let _ =
  set_game game4;
  buy_fish_game game4 Turtle 5;
  health_tank (get_tank game4) (-30.)

let game5 = start_game 3

let _ =
  set_game game5;
  buy_fish_game game5 Goldfish 2;
  buy_fish_game game5 Goldfish 2;
  med_game_species game5 Goldfish

let game6 = start_game 1

let _ =
  set_game game6;
  buy_fish_game game6 Clownfish 2;
  health_tank (get_tank game6) (-51.);
  buy_fish_game game6 Goldfish 2;
  growth_tank (get_tank game6)

let game7 = start_game 4

let _ =
  set_game game7;
  buy_fish_game game7 Shark 1;
  (*Set shark health at 10*)
  health_tank (get_tank game7) (-90.);
  buy_fish_game game7 Clownfish 10

let array = shark_dinner (get_tank game7)

(*if the shark eats, their health increases*)
let fish_eaten = array.(2) > 0
let shark_health = get_health game7 Shark > 10.

(*Symbiosis*)
let game8 = start_game 6

let _ =
  set_game game8;
  buy_fish_game game8 Shark 2;
  buy_fish_game game8 Remora 10;
  end_of_round game8

let currency_tests =
  [
    ("price of 1 goldfish" >:: fun _ -> assert_equal 2. (price_fish Goldfish 1));
    ("price of 10 sharks" >:: fun _ -> assert_equal 200. (price_fish Shark 10));
    ("price of 3 remorae" >:: fun _ -> assert_equal 24. (price_fish Remora 3));
    ( "price_fish exception" >:: fun _ ->
      assert_raises (Failure "Invalid") (fun () -> price_fish Huh 3) );
    ("Extinct Shark" >:: fun _ -> assert_equal true (species_extinct game Shark));
    ( "Not Extinct Shark" >:: fun _ ->
      assert_equal false (species_extinct game2 Shark) );
    ("Start w/ 100" >:: fun _ -> assert_equal 100. (get_playermoney game));
    ("Cost 40." >:: fun _ -> assert_equal 60. (get_playermoney game2));
    ( "Healthy fish species" >:: fun _ ->
      assert_equal 100. (get_health game Goldfish) );
    ("Sick sharks" >:: fun _ -> assert_equal 20. (get_health game2 Shark));
    ( "Feed hungry anemone" >:: fun _ ->
      assert_equal 100. (get_health game3 Anemone) );
    ( "not enough money to buy 100 anemone" >:: fun _ ->
      assert_equal true (buy_broke game3 Anemone 100) );
    ( "enough money to buy" >:: fun _ ->
      assert_equal false (buy_broke game Turtle 0) );
    ( "not enough money to buy medicine" >:: fun _ ->
      assert_equal true (med_broke game3) );
    ( "enough money to buy medicine" >:: fun _ ->
      assert_equal false (med_broke game) );
    ( "not enough money to buy food" >:: fun _ ->
      assert_equal true (feed_broke game3 100) );
    ( "enough money to buy food" >:: fun _ ->
      assert_equal false (feed_broke game 10) );
    ("empty game earnings" >:: fun _ -> assert_equal 0. (earnings game));
    ( "earnings from 7 clownfish and 7 anemone" >:: fun _ ->
      assert_equal 91. (earnings game3) );
  ]

let fish_tests =
  [ ("make empty goldfish" >:: fun _ -> assert_equal 0 (get_age goldfish)) ]

let health_tests =
  [
    ("Extinct Shark" >:: fun _ -> assert_equal true (species_extinct game Shark));
    ( "Not Extinct Shark" >:: fun _ ->
      assert_equal false (species_extinct game2 Shark) );
    ("Start w/ 100" >:: fun _ -> assert_equal 100. (get_playermoney game));
    ("Cost 40." >:: fun _ -> assert_equal 60. (get_playermoney game2));
    ( "Healthy fish species" >:: fun _ ->
      assert_equal 100. (get_health game Goldfish) );
    ("Sick sharks" >:: fun _ -> assert_equal 20. (get_health game2 Shark));
    ( "Feed hungry anemone" >:: fun _ ->
      assert_equal 100. (get_health game3 Anemone) );
    ("health_tank -30." >:: fun _ -> assert_equal 70. (get_health game4 Turtle));
  ]

let population_tests =
  [
    ("age 10 remora" >:: fun _ -> assert_equal 10 (get_age remora));
    ( "growth_tank sick Clownfish" >:: fun _ ->
      assert_equal 1 (get_species_population game6 Clownfish) );
    ( "growth_tank healthy goldfish" >:: fun _ ->
      assert_equal 3 (get_species_population game6 Goldfish) );
  ]

let game_tests =
  [
    ( "predator_fish false" >:: fun _ ->
      assert_equal false (predator_species game Goldfish) );
    ( "predator_fish true" >:: fun _ ->
      assert_equal true (predator_species game Shark) );
    ("add 10 remora" >:: fun _ -> assert_equal 10 (get_num remora));
    ("10 round maximum" >:: fun _ -> assert_equal 10 (get_max_rounds game));
    ("5 round maximum" >:: fun _ -> assert_equal 5 (get_max_rounds game2));
    ( "End Score Calculation" >:: fun _ ->
      assert_equal true (end_score game2 > 0) );
    ("Shark_dinner" >:: fun _ -> assert_equal fish_eaten shark_health);
    ( "End Score Calculation" >:: fun _ ->
      assert_equal true (end_score game2 > 0) );
  ]

let print_tests =
  [
    ( "String of Goldfish" >:: fun _ ->
      assert_equal "Goldfish" (string_of_species Goldfish) );
    ( "String of Anemone" >:: fun _ ->
      assert_equal "Anemone" (string_of_species Anemone) );
    ( "String of Clownfish" >:: fun _ ->
      assert_equal "Clownfish" (string_of_species Clownfish) );
    ( "String of Turtle" >:: fun _ ->
      assert_equal "Turtle" (string_of_species Turtle) );
    ( "String of Remora" >:: fun _ ->
      assert_equal "Remora" (string_of_species Remora) );
    ( "String of Shark" >:: fun _ ->
      assert_equal "Shark" (string_of_species Shark) );
    ("Exception Huh" >:: fun _ -> assert_equal "Huh" (string_of_species Huh));
    ( "Plural of Goldfish" >:: fun _ ->
      assert_equal "Goldfish" (plural_species Goldfish) );
    ( "Plural of Anemone" >:: fun _ ->
      assert_equal "Anemones" (plural_species Anemone) );
    ( "Plural of Clownfish" >:: fun _ ->
      assert_equal "Clownfish" (plural_species Clownfish) );
    ( "Plural of Turtle" >:: fun _ ->
      assert_equal "Turtles" (plural_species Turtle) );
    ( "Plural of Remora" >:: fun _ ->
      assert_equal "Remorae" (plural_species Remora) );
    ("Plural of Shark" >:: fun _ -> assert_equal "Sharks" (plural_species Shark));
    ("Exception Huh" >:: fun _ -> assert_equal "Huh" (plural_species Huh));
  ]

let suite =
  "test suite"
  >::: List.flatten
         [
           currency_tests;
           fish_tests;
           health_tests;
           population_tests;
           game_tests;
           print_tests;
         ]

let () = run_test_tt_main suite
