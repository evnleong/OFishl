(*opening Modules in lib*)
open Final_project
open Userinput

(*test terminal output*)
let () =
  ANSITerminal.print_string [ ANSITerminal.cyan ]
    "Build your own aquarium! Start the game with $100, use \n\
     it to buy more fish and tanks. Give your aquatic friends cute names and \
     watch them grow.\n\
     Each round simulates one day, the game ends after 3 rounds\n\n\
     Be careful: each day you have a limited amount of actions you can take! \n\n\
     Start Game:\n"

(*Create a new game w/ 3 rounds,g represents current game state*)
let g = Game.start_game 3
let user_prompt () = read_line ()
let command string = parse_input string

(*Round 1:*)
let _ = print_endline (Game.start_round_print g)

(*Continously prompt user until invalid option entered or out of actions *)
let rec action num_actions (g : Game.player_state) : Game.player_state =
  try
    if num_actions = 0 then raise Exit
    else
      print_endline
        ("What would you like to do today? \n You currently have "
       ^ string_of_int num_actions
       ^ " action(s) left today. \n\
         \ Type (Buy, Tanks, Wallet) or Ctrl +C to Exit");
    let response = command (user_prompt ()) in
    match response with
    | Wallet ->
        print_endline ("you have $" ^ string_of_int (Game.print_playermoney g));
        action num_actions g
    | Buy ->
        print_endline
          "What type of fish would you like? (Goldfish, Pufferfish, Shark)";
        let prompt = read_line () in
        let fish_type =
          match prompt with
          | "Pufferfish" -> Game.pufferfish
          | "Shark" -> Game.shark
          | "Goldfish" -> Game.goldfish
          | _ -> failwith "Invalid fish type"
        in
        let fish_cost =
          match prompt with
          | "Pufferfish" -> 4
          | "Shark" -> 10
          | "Goldfish" -> 2
          | _ -> failwith "e"
        in
        print_endline "Give a name for your new fish >";
        let fish = fish_type (user_prompt ()) in
        let updatedgamestate =
          Game.cost (Game.add_fish_game g fish) fish_cost
        in
        action (num_actions - 1) updatedgamestate
    | View_Tanks ->
        print_endline (Game.end_round_print g);
        action num_actions g
    | _ -> raise Exit
  with Exit ->
    print_endline "Invalid Option or Out of Moves";
    g

let g = action 2 g
let _ = print_endline (Game.end_round_print g)
let g = Game.end_of_round g

(*Round 2:*)
let _ = print_endline (Game.start_round_print g)
let g = action 2 g
let _ = print_endline (Game.end_round_print g)
let g = Game.end_of_round g

(*Round 3:*)
let _ = print_endline (Game.start_round_print g)
let g = action 2 g
let _ = print_endline (Game.end_round_print g)
let _ = print_endline "bye-bye"
