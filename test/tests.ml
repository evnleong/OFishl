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
open Userinput

let goldfish = make_fish Goldfish Pellet 1.4 0.6
let remora = make_fish Remora Pellet 1. 0.8

(* FUNCTIONS TO AID TESTING *)
let sum_prey_eaten (prey : prey_record) : int = 
  prey.goldfish + prey.anemone + prey.clownfish + prey.turtle

let and_prey_eaten (prey : prey_record) (g : int) (a : int) 
    (c : int) (t : int) : bool = 
  prey.goldfish = g
  && prey.anemone = a 
  && prey.clownfish = c 
  && prey.turtle = t

(* SAMPLE GAMES *)

(* Game 1: Empty game *)
let game1 = start_game 10

let _ =
  set_game game1;
  age_tank (get_tank game1)

(* Game 2: Remorae and unhealthy sharks symbiosis *)
let game2 = start_game 5

let _ =
  set_game game2;
  buy_fish_game game2 Shark 2;
  health_tank_species (get_tank game2) Shark (-80.);
  add_fish remora 10

(* Game 3: Clownfish and anemone *)
let game3 = start_game 10

let _ =
  set_game game3;
  buy_fish_game game3 Clownfish 7;
  buy_fish_game game3 Anemone 7;
  health_tank_species (get_tank game3) Anemone (-50.);
  feed_fish_game game3 Anemone (7 * 50)

(* Game 4: Only turtles *)
let game4 = start_game 3

let _ =
  set_game game4;
  buy_fish_game game4 Turtle 5;
  health_tank (get_tank game4) (-30.)

(* Game 5: Only goldfish *)
let game5 = start_game 3

let _ =
  set_game game5;
  buy_fish_game game5 Goldfish 2;
  buy_fish_game game5 Goldfish 2

(* Game 6: Clownfish and goldfish *)
let game6 = start_game 1

let _ =
  set_game game6;
  buy_fish_game game6 Clownfish 2;
  health_tank (get_tank game6) (-51.);
  buy_fish_game game6 Goldfish 2;
  growth_tank (get_tank game6)

(* Game 7: 1 shark and 10 clownfish *)
let game7 = start_game 4

let _ =
  set_game game7;
  buy_fish_game game7 Shark 1;
  buy_fish_game game7 Clownfish 10

let shark7 = 
  let prey7 = get_eaten_prey game7 in 
  (prey7.clownfish = 1
  && prey7.anemone = 0
  && prey7.goldfish = 0
  && prey7.turtle = 0)
  && get_health game7 Shark = 100.


(* Game 8: Only sharks; lose health for three rounds; add goldfish *)
let game8 = start_game 4

let _ =
  set_game game8;
  buy_fish_game game8 Shark 4

let shark8 = 
  let _ = get_eaten_prey game8 in 
  let _ = get_eaten_prey game8 in 
  let prey8 = get_eaten_prey game8 in 
  and_prey_eaten prey8 0 0 0 0
  && get_health game8 Shark = 40.

let _ = 
  buy_fish_game game8 Goldfish 2

let shark8' = 
  let prey8 = get_eaten_prey game8 in 
  and_prey_eaten prey8 2 0 0 0 
  && get_health game8 Shark = 30.

(* Game 9: Buy one of every fish; shark successively eats *)
let game9 = start_game 3 

let _ = 
  set_game game9;
  buy_fish_game game9 Goldfish 1;
  buy_fish_game game9 Anemone 1;
  buy_fish_game game9 Clownfish 1;
  buy_fish_game game9 Remora 1;
  buy_fish_game game9 Turtle 1;
  buy_fish_game game9 Shark 1 
  
let shark9 = 
  let prey9 = get_eaten_prey game9 in 
    (prey9.goldfish = 1
    || prey9.anemone = 1
    || prey9.clownfish = 1 
    || prey9.turtle = 1)
    && (sum_prey_eaten prey9 = 1)
  
let shark9' = 
  let prey9' = get_eaten_prey game9 in 
  let prey9'' = get_eaten_prey game9 in 
  let prey9''' = get_eaten_prey game9 in 
  let prey9'''' = get_eaten_prey game9 in 
  sum_prey_eaten prey9' = 1
  && sum_prey_eaten prey9'' = 1
  && sum_prey_eaten prey9''' = 1 
  && sum_prey_eaten prey9'''' = 0 

(* Game 10: 2 sharks eat 2 goldfish, then go hungry *)
let game10 = start_game 3 

let _ = 
  set_game game10; 
  buy_fish_game game10 Shark 2;
  buy_fish_game game10 Goldfish 2

let shark10 = 
  let prey10 = get_eaten_prey game10 in 
  and_prey_eaten prey10 2 0 0 0
  && get_health game10 Shark = 100.

let shark10' = 
  let prey10' = get_eaten_prey game10 in 
  and_prey_eaten prey10' 0 0 0 0 
  && get_health game10 Shark = 90.

(* Game 11: Game with 3 rounds; force game to end *)
let game11 = start_game 3 

let _ = 
  set_game game11; 
  end_of_round game11; 
  end_of_round game11; 
  end_of_round game11

(* Game 12: Force goldfish to go extinct *)
let game12 = start_game 3 

let _ = 
  set_game game12; 
  buy_fish_game game12 Goldfish 10;
  health_tank_species (get_tank game12) Goldfish  (-100.);
  end_of_round game12

let goldfish12 = 
  get_health game12 Goldfish = 100. 
  && get_species_num game12 Goldfish = 0 
  && species_extinct game12 Goldfish = true  

(* Game 13: Population successively grows *)
let game13 = start_game 5

let num1 = 
  set_game game13; 
  buy_fish_game game13 Goldfish 50;
  end_of_round game13; 
  get_species_num game13 Goldfish 

let num2 = 
  end_of_round game13;
  get_species_num game13 Goldfish 

let num3 = 
  end_of_round game13; 
  get_species_num game13 Goldfish 

let goldfish13 = (num2 > num1) && (num3 > num2)

(* Game 14: Population successively declines *)
let game14 = start_game 5

let num1 = 
  set_game game14; 
  buy_fish_game game14 Goldfish 50;
  health_tank_species (get_tank game14) Goldfish  (-50.);
  end_of_round game14; 
  get_species_num game14 Goldfish 

let num2 = 
  end_of_round game14;
  get_species_num game14 Goldfish 

let num3 = 
  end_of_round game14; 
  get_species_num game14 Goldfish 

let goldfish14 = (num2 < num1) && (num3 < num2)

(* Functions related to money, earning, buying *)
let money_tests = [
  (* price_fish tests *)
  ( "Price of 1 goldfish" >:: fun _ -> assert_equal 2. (price_fish Goldfish 1) );
  ( "Price of 0 goldfish" >:: fun _ -> assert_equal 0. (price_fish Goldfish 0) );
  ( "Price of 10 sharks" >:: fun _ -> assert_equal 200. (price_fish Shark 10) );
  ( "Price of 3 remorae" >:: fun _ -> assert_equal 24. (price_fish Remora 3) );
  ( "Price of 5 anemone" >:: fun _ -> assert_equal 20. (price_fish Anemone 5) );
  ( "Price of 8 clownfish" >:: fun _ -> assert_equal 80. (price_fish Clownfish 8) );
  ( "Price of 7 turtles" >:: fun _ -> assert_equal 105. (price_fish Turtle 7) );
  ( "Price_fish exception" >:: fun _ ->
    assert_raises (Failure "Invalid") (fun () -> price_fish Huh 3) );

  (* get_playermoney tests *)
  ("Playermoney start of game1" >:: fun _ -> assert_equal 100. (get_playermoney game1));
  ("Playermoney game2" >:: fun _ -> assert_equal 60. (get_playermoney game2));
  ("Playermoney game3" >:: fun _ -> assert_equal ~-.33. (get_playermoney game3));
  ("Playermoney game4" >:: fun _ -> assert_equal 25. (get_playermoney game4));

  (* broke tests *)
  ( "Broke game3 buy 100 anemone" >:: fun _ ->
    assert_equal true (buy_broke game3 Anemone 100) );
  ( "Not broke game1 0 turtles" >:: fun _ ->
    assert_equal false (buy_broke game1 Turtle 0) );
  ( "Broke medicine game3" >:: fun _ ->
    assert_equal true (med_broke game3) );
  ( "Not broke medicine game1" >:: fun _ ->
    assert_equal false (med_broke game1) );
  ( "Broke food game3 100" >:: fun _ ->
    assert_equal true (feed_broke game3 100) );
  ( "Not broke food game1 10" >:: fun _ ->
    assert_equal false (feed_broke game1 10) );

  (* earning tests *)
  ( "Earnings empty game" >:: fun _ -> assert_equal 0. (earnings game1) );
  ( "Earnings game3" >:: fun _ ->
    assert_equal 35. (earnings game3) );
]

(* Functions and actions that manipulate health: medicine, food, shark eating, symbiosis *)
let health_tests = [
  (* check health tests *)
  ( "Health goldfish game1" >:: fun _ ->
    assert_equal 100. (get_health game1 Goldfish) );
  ( "Health sharks game2" >:: fun _ -> assert_equal 20. (get_health game2 Shark));
  ( "Health goldfish game1" >:: fun _ ->
    assert_equal 100. (get_health game1 Goldfish) );
  ( "Health turtle game4" >:: fun _ -> assert_equal 70. (get_health game4 Turtle) );

  (* feed tests *)

  (* symbiosis tests *)

  (* shark eat tests *)
  ( "Shark eat game7" >:: fun _ -> assert_equal true shark7 );
  ( "Shark eat game8 hungry" >:: fun _ -> assert_equal true shark8 );  
  ( "Shark eat game8 2 goldfish" >:: fun _ -> assert_equal true shark8' ); 
  ( "Shark eat first game9" >:: fun _ -> assert_equal true shark9 );
  ( "Shark eat successively game9" >:: fun _ -> assert_equal true shark9' );
  ( "Shark eat game10 2 goldfish" >:: fun _ -> assert_equal true shark10 );
  ( "Shark eat game10 hungry" >:: fun _ -> assert_equal true shark10' );
]

(* Tests to do with population numbers: extinct, growth *)
let population_tests = [
  (* extinct tests *)
  ( "Extinct shark game1" >:: fun _ -> assert_equal true (species_extinct game1 Shark));
  ( "Not extinct shark game2" >:: fun _ ->
  assert_equal false (species_extinct game2 Shark) );
  ( "Extinct goldfish game2" >:: fun _ -> assert_equal true (species_extinct game3 Goldfish));
  ( "Not extinct clownfish game3" >:: fun _ ->
      assert_equal false (species_extinct game3 Clownfish) ); 
  ( "Extinct goldfish game12" >:: fun _ -> assert_equal true goldfish12 );  

  (* population number tests *)
  ( "Growth sick clownfish game6" >:: fun _ ->
    assert_equal 1 (get_species_num game6 Clownfish) );
  ( "Growth healthy goldfish game6" >:: fun _ ->
    assert_equal 3 (get_species_num game6 Goldfish) );
  ( "Growth healthy goldfish successive rounds game13" >:: fun _ ->
    assert_equal true goldfish13 );
  ( "Growth sick goldfish successive rounds game14" >:: fun _ ->
    assert_equal true goldfish14 );
  ]


(* Tests for other functions manipulating fish properties *)
let fish_tests = [
  ("Make empty goldfish" >:: fun _ -> assert_equal 0 (get_age goldfish)); 
  ( "Predator goldfish false" >:: fun _ ->
  assert_equal false (predator_species game1 Goldfish) );
  ( "Predator shark true" >:: fun _ ->
  assert_equal true (predator_species game1 Shark) );
  ( "Age of remorae" >:: fun _ -> assert_equal 0 (get_age remora) );
  ]

(* Tests for end of game and end of round *)
let game_tests = [
  ( "Number of remorae" >:: fun _ -> assert_equal 10 (get_num remora));

  (* Max round tests *)
  ( "10 round maximum" >:: fun _ -> assert_equal 10 (get_max_rounds game1));
  ( "5 round maximum" >:: fun _ -> assert_equal 5 (get_max_rounds game2));

  (* End score *)
  ( "End score game2" >:: fun _ ->
    assert_equal true (end_score game2 > 0) );
  ( "End score calculation" >:: fun _ ->
    assert_equal 1000 (end_score game1) );

  (* game_ended *)
  ( "Game ended game11" >:: fun _ ->
    assert_equal true (game_ended game11) );
  ]

let string_tests = [
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

(* USER INPUT TESTS *)
let user_input_tests = [
  (* parse tests *)
  ( "Parse empty string" >:: fun _ ->
    assert_equal [] (parse "") );  
  ( "Parse ABCDEF" >:: fun _ ->
    assert_equal [ "ABCDEF" ] (parse "ABCDEF") );
  ( "Parse abcdef" >:: fun _ ->
    assert_equal [ "ABCDEF" ] (parse "abcdef") );
  ( "Parse whitespace" >:: fun _ ->
    assert_equal [] (parse "   ") );
  ( "Parse hi hello" >:: fun _ ->
    assert_equal ["HI"; "HELLO"] (parse "hi hello") );
  ( "Parse camels are slay" >:: fun _ ->
    assert_equal ["CAMELS"; "ARE"; "SLAY"] (parse "camels are slay") );
  ( "Parse words with whitespace" >:: fun _ ->
    assert_equal ["CAMELS"; "ARE"; "SLAY"] (parse "   camels   are  slay  ") );

  (* parse_input tests *)
  ( "Parse input empty string" >:: fun _ ->
      assert_equal Dunno (parse_input "") );
  ( "Parse input whitespace" >:: fun _ ->
      assert_equal Dunno (parse_input "  ") );
  ( "Parse input invalid one word" >:: fun _ ->
    assert_equal Dunno (parse_input "abcdef") );
  ( "Parse input invalid two words" >:: fun _ ->
    assert_equal Dunno (parse_input "two words") );
  ( "Parse input invalid Fe ed" >:: fun _ ->
    assert_equal Dunno (parse_input "Fe ed") );
  ( "Parse input Fe ed" >:: fun _ ->
    assert_equal Dunno (parse_input "Fe ed") );
  ( "Parse input invalid Manual 5" >:: fun _ ->
    assert_equal Dunno (parse_input "Manual 5") );

  ( "Parse input feed" >:: fun _ ->
      assert_equal Feed (parse_input "Feed") );
  ( "Parse input feed with whitespace" >:: fun _ ->
      assert_equal Feed (parse_input " Feed  ") );
  ( "Parse input feed all caps" >:: fun _ ->
      assert_equal Feed (parse_input "FEED") );
  ( "Parse input feed lowercase" >:: fun _ ->
      assert_equal Feed (parse_input "feed") );
  ( "Parse input feed alternate uppercase lowercase" >:: fun _ ->
      assert_equal Feed (parse_input "fEeD") );

  ( "Parse input Buy" >:: fun _ ->
    assert_equal Buy (parse_input "Buy") );
  ( "Parse input buy all caps" >:: fun _ ->
    assert_equal Buy (parse_input "BUY") );
  ( "Parse input Medicine" >:: fun _ ->
    assert_equal Medicine (parse_input "Medicine") );
  ( "Parse input Tank" >:: fun _ ->
    assert_equal View_Tanks (parse_input "Tank") );
  ( "Parse input Manual" >:: fun _ ->
    assert_equal Manual (parse_input "Manual") );
  ( "Parse input Pass" >:: fun _ ->
    assert_equal Pass (parse_input "Pass") );

  (* parse_species tests *)
  ( "Parse species Goldfish" >:: fun _ ->
    assert_equal Goldfish (parse_species "Goldfish") );
  ( "Parse species goldfish all caps" >:: fun _ ->
    assert_equal Goldfish (parse_species "GOLDFISH") );
  ( "Parse species GoLdFISH" >:: fun _ ->
    assert_equal Goldfish (parse_species "GoLdFISH") );
  ( "Parse species goldfish with whitespace" >:: fun _ ->
    assert_equal Goldfish (parse_species "   Goldfish  ") );
  ( "Parse species Remora" >:: fun _ ->
    assert_equal Remora (parse_species "Remora") );
  ( "Parse species anemone" >:: fun _ ->
    assert_equal Anemone (parse_species "Anemone") );
  ( "Parse species clownfish" >:: fun _ ->
    assert_equal Clownfish (parse_species "Clownfish") );
  ( "Parse species turtle" >:: fun _ ->
    assert_equal Turtle (parse_species "Turtle") );
  ( "Parse species shark" >:: fun _ ->
    assert_equal Shark (parse_species "Shark") );
  ( "Invalid species input is caught with Huh constructor" >:: fun _ ->
    assert_equal Huh (parse_species "awsjfh") );

    
  (* parse species int tests*)
  ( "parse_species_int Goldfish 1" >:: fun _ ->
    assert_equal (Goldfish, 1) (parse_species_int "Goldfish 1") );
  ( "parse_species_int Goldfish 1 alternating caps" >:: fun _ ->
    assert_equal (Goldfish, 1) (parse_species_int "GOlDfIsH 1") );
  ( "parse_species_int Goldfish -2" >:: fun _ ->
    assert_equal (Goldfish, -2) (parse_species_int "Goldfish -2") );
  ( "parse_species_int Goldfish 0" >:: fun _ ->
    assert_equal (Goldfish, 0) (parse_species_int "Goldfish 0") );
  ( "parse_species_int Goldfish 5 whitespace" >:: fun _ ->
    assert_equal (Goldfish, 5) (parse_species_int "  Goldfish    5   ") );
  ( "parse_species_int anemone 1 all caps" >:: fun _ ->
    assert_equal (Anemone, 1) (parse_species_int "ANEMONE 1") );
  ( "parse_species_int anemone 5" >:: fun _ ->
    assert_equal (Anemone, 5) (parse_species_int "Anemone 5") );
  ( "parse_species_int turtle 5" >:: fun _ ->
    assert_equal (Turtle, 5) (parse_species_int "Turtle 5") );
  ( "parse_species_int remora 10" >:: fun _ ->
    assert_equal (Remora, 10) (parse_species_int "Remora 10") );
  ( "parse_species_int shark 5" >:: fun _ ->
    assert_equal (Shark, 5) (parse_species_int "Shark 5") );

  ( "parse_species_int invalid input one word" >:: fun _ ->
    assert_equal (Huh, 0) (parse_species_int "awsjfh") );
  ( "parse_species_int invalid Goldfish1" >:: fun _ ->
    assert_equal (Huh, 0) (parse_species_int "Goldfish1") );
  ( "parse_species_int invalid Turtle 5 hello" >:: fun _ ->
    assert_equal (Huh, 0) (parse_species_int "Turtle 5 hello") );
  ( "parse_species_int invalid Turtles 5" >:: fun _ ->
    assert_equal (Huh, 0) (parse_species_int "Turtles 5") );
  ( "parse_species_int invalid empty string" >:: fun _ ->
    assert_equal (Huh, 0) (parse_species_int "") );
  ( "parse_species_int invalid whitespace" >:: fun _ ->
    assert_equal (Huh, 0) (parse_species_int "  ") );
  ( "parse_species_int invalid Turtle whitespace" >:: fun _ ->
    assert_equal (Huh, 0) (parse_species_int "Turtle  ") );
  ( "parse_species_int invalid Turtle one" >:: fun _ ->
    assert_equal (Huh, 0) (parse_species_int "Turtle one") );
  ( "parse_species_int invalid Goldfish Turtle" >:: fun _ ->
    assert_equal (Huh, 0) (parse_species_int "Goldfish Turtle") );
  ( "parse_species_int invalid 1 Goldfish" >:: fun _ ->
    assert_equal (Huh, 0) (parse_species_int "1 Goldfish") );
  ( "parse_species_int invalid Goldfish comma 1" >:: fun _ ->
    assert_equal (Huh, 0) (parse_species_int "Goldfish, 1") );
  ( "parse_species_int invalid Goldfish --2" >:: fun _ ->
    assert_equal (Huh, 0) (parse_species_int "Goldfish --2") );
  ( "parse_species_int invalid hello 2" >:: fun _ ->
    assert_equal (Huh, 0) (parse_species_int "hello 2") ); 
  ]


let suite =
  "test suite"
  >::: List.flatten
         [
           money_tests;
           user_input_tests;
           health_tests;
           population_tests;
           fish_tests;
           game_tests;
           string_tests;
         ]

let () = run_test_tt_main suite
