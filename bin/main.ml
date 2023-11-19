(* Opening Modules in lib. *)
open Final_project
open Userinput

(** Message for erroneous user inputs. *)
let dunno () = print_endline "Oops, we didn't catch that."

(** Buy fish. *)
let rec buy (num_actions : int) (g : Game.game_state) : unit =
  print_endline
    "\n\
    \ Buy a number of fish of a species. For example, \n\
    \  to add 10 golfish to your tank, type \"Goldfish 10\". Price per fish: \n\
    \  golfish $5, pufferfish $10, shark $20.";

  match parse_species_int (read_line ()) with
  | s, n ->
      if s = Huh then (
        dunno ();
        action num_actions g)
      else Game.buy_fish_game g s n;
      action (num_actions - 1) g

(** Feed n pellets to fish population. *)
and feed (num_actions : int) (g : Game.game_state) : unit =
  print_endline
    "\n\
    \ Feed pellets to a species in your tank. For example, \n\
    \  to feed the goldfish in your tank 10 pellets, type \"Goldfish 10\". \n\
    \  One pellet costs $0.5, and if a species  has N fish, then it takes \n\
    \  N pellets to increase its health by one point.";

  match parse_species_int (read_line ()) with
  | s, n ->
      if s = Huh then (
        dunno ();
        action num_actions g)
      else Game.feed_fish_game g s n;
      action (num_actions - 1) g

(** Give medicine to a fish population. *)
and medicine (num_actions : int) (g : Game.game_state) : unit =
  print_endline
    "\n\
    \ Give medicine to a species in your tank to boost its health by 30 points.\n\
    \     For example, to give medicine to the goldfish in your tank, type \n\
    \     \"Goldfish\". Medicine has a flat price of $50.";

  match read_line () |> parse_species with
  | Huh ->
      dunno ();
      action num_actions g
  | s ->
      Game.med_game_species g s;
      action (num_actions - 1) g

and action num_actions (g : Game.game_state) : unit =
  try
    if num_actions = 0 then raise Exit
    else
      ANSITerminal.print_string [ ANSITerminal.cyan ]
        ("\n What would you like to do today? \n You currently have "
       ^ string_of_int num_actions
       ^ " action(s) left today. \n\
         \ Type (Buy, Feed, Medicine, Tanks, Wallet, Instructions) or Ctrl +C \
          to Exit  \n");
    let response = parse_input (read_line ()) in
    match response with
    | Buy -> buy num_actions g
    | Feed -> feed num_actions g
    | Medicine -> medicine num_actions g
    | View_Tanks ->
        Game.print_fish g;
        action num_actions g
    | Wallet ->
        print_endline ("You have $" ^ string_of_float (Game.get_playermoney g));
        action num_actions g
    | Instructions -> failwith "UNIMPLEMENTED"
    | Dunno ->
        dunno ();
        action num_actions g
  with Exit -> print_endline "Next day...."

let () =
  ANSITerminal.print_string [ ANSITerminal.cyan ]
    "Build your own aquarium! Start the game with $100, use \n\
     it to buy more fish and tanks.\n\
     Each round simulates one day, the game ends after 3 rounds\n\n\
     Be careful: each day you have a limited amount of actions you can take! \n\n\
     Start Game:\n";
  let game = Game.start_game 3 in
  Game.set_game game;
  for i = 1 to Game.get_max_rounds game do
    print_endline ("Starting round:" ^ string_of_int i);
    action 2 game
  done;
  ANSITerminal.print_string [ ANSITerminal.cyan ]
    "\n Thanks for playing! \n Here's your game summary: \n";
  Game.print_fish game

(* OLD CODE BELOW *)

(* Create a new game w/ 3 rounds, g represents current game state. *)
(*let g = Game.start_game 3
  let user_prompt () = read_line ()
  let command string = string |> String.uppercase_ascii |> parse_input

  (* Round 1: *)
  let _ = print_endline (Game.start_round_print g)

  (* Helper for updategame. *)
  let updatehelper g c f =
    Game.cost (Game.add_fish_game g f) c

  (* Helper for action. *)
  let updategame g name response =
    match response with
    | Pufferfish -> name |> Game.pufferfish |> updatehelper g 4
    | Shark -> name |> Game.shark |> updatehelper g 10
    | Goldfish -> name |> Game.goldfish |> updatehelper g 2
    | _ -> failwith "Invalid"

  (* Continously prompt user until invalid option entered or out of actions. *)
  let rec action num_actions (g : Game.game_state) : Game.game_state =
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
          print_endline ("you have $" ^ string_of_float (Game.print_playermoney g));
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
  let _ = print_endline "bye-bye"*)
