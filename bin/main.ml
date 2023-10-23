(* Opening Modules in lib. *)
open Final_project
open Userinput

(* Test terminal output. *)
let () =
  ANSITerminal.print_string [ ANSITerminal.cyan ]
    "Build your own aquarium! Start the game with $100, use \n\
     it to buy more fish and tanks. Give your aquatic friends cute names and \
     watch them grow.\n\
     Each round simulates one day, the game ends after 3 rounds\n\n\
     Be careful: each day you have a limited amount of actions you can take! \n\n\
     Start Game:\n"

(* Create a new game w/ 3 rounds,g represents current game state. *)
let g = Game.start_game 3
let user_prompt () = read_line ()
let command string = string |> String.uppercase_ascii |> parse_input

(* Round 1: *)
let _ = print_endline (Game.start_round_print g)

(* Helper for updategame. *)
let updatehelper g c f = 
  Game.cost (Game.add_fish_game g f) c

(* Helper for num_actions. *)
let updategame g name response = 
  match response with
  | Pufferfish -> name |> Game.pufferfish |> updatehelper g 4
  | Shark -> name |> Game.shark |> updatehelper g 10
  | Goldfish -> name |> Game.goldfish |> updatehelper g 2
  | _ -> failwith "Invalid"

(* Continously prompt user until invalid option entered or out of actions. *)
let rec action num_actions (g : Game.player_state) : Game.player_state =
  try
    if num_actions = 0 then raise Exit
    else
      print_endline
        ("\n What would you like to do today? \n You currently have "
       ^ string_of_int num_actions
       ^ " action(s) left today. \n\
         \ Type (Buy, Tanks, Wallet, Feed) or Ctrl +C to Exit");
    let response = command (user_prompt ()) in
    match response with
    | Wallet ->
        print_endline ("you have $" ^ string_of_int (Game.print_playermoney g));
        action num_actions g
    | Buy ->
        print_endline
          ( "You can buy a goldfish for $2, a pufferfish for $4, or a shark for $10."
          ^ "\n What type of fish would you like? ");
        let response = command (read_line ()) in
        begin match response with
        | Dunno -> 
          print_endline "Oops, try again.";
          action num_actions g
        | _ ->
          print_endline "Give a name for your new fish >";
          action (num_actions - 1) (updategame g (user_prompt ()) response)
        end
    | View_Tanks ->
        print_endline (Game.end_round_print g);
        action num_actions g
    | Feed -> 
        print_endline "\n Your fish are getting hungry. Would you like to spend $10 to feed them? Type Yes or No.\n";
        let updatedgamestate =
          if (read_line () |> String.uppercase_ascii) = "YES" then
          Game.cost (Game.feed_game g) 10 else g 
        in
        action (num_actions - 1) updatedgamestate
    | Dunno -> 
        print_endline "Oops, try again.";
        action num_actions g
    | _ -> raise Exit
  with Exit ->
    print_endline "Invalid Option or Out of Moves";
    g

let g = action 2 g
let _ = print_endline (Game.end_round_print g)
let g = Game.end_of_round g

(* Round 2: *)
let _ = print_endline (Game.start_round_print g)
let g = action 2 g
let _ = print_endline (Game.end_round_print g)
let g = Game.end_of_round g

(* Round 3: *)
let _ = print_endline (Game.start_round_print g)
let g = action 2 g
let _ = print_endline (Game.end_round_print g)
let _ = print_endline "bye-bye"
