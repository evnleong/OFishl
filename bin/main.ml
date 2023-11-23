(* Opening Modules in lib. *)
open Final_project
open Userinput

(** Message for erroneous user inputs. *)
let dunno () = print_endline "Oops, we didn't catch that."

(** Buy fish. *)
let rec buy (num_actions : int) (g : Game.game_state) : unit =
  print_endline
    ("\n  You currently have $"
    ^ string_of_float (Game.get_playermoney g)
    ^ "\n\
      \  Buy some fish of a species to add to your tank. \n\
      \  Example: To buy 10 goldfish, type \"Goldfish 10\". \n\
      \  Price per fish: Goldfish $2, Anemone $4, Remora $8, \n\
      \  Clownfish $10, Turtle $15, Shark $20.");

  match parse_species_int (read_line ()) with
  | s, n ->
      if s = Huh then (
        dunno ();
        action num_actions g)
      else if Game.buy_broke g s n then (
        print_endline "\n  You do not have enough money.";
        action num_actions g)
      else Game.buy_fish_game g s n;
      action (num_actions - 1) g

(** Feed n pellets to fish population. *)
and feed (num_actions : int) (g : Game.game_state) : unit =
  print_endline
    "\n\
    \  Feed pellets to a species in your tank. \n\
    \  Example: To feed your goldfish 10 pellets, type \"Goldfish 10\". \n\
    \  One pellet costs $0.1, and feeding n pellets to a species with N fish \n\
    \  increases its health by n/N points.";

  match parse_species_int (read_line ()) with
  | s, n ->
      if s = Huh then (
        dunno ();
        action num_actions g)
      else if Game.predator_species g s then (
        print_endline ("\n  " ^ Game.plural_species s ^ " do not eat pellets.");
        action num_actions g)
      else if Game.species_extinct g s then (
        print_endline
          ("\n  You do not have any "
          ^ (s |> Game.plural_species |> String.lowercase_ascii)
          ^ " in your tank.");
        action num_actions g)
      else if Game.feed_broke g n then (
        print_endline "\n  You do not have enough money.";
        action num_actions g)
      else Game.feed_fish_game g s n;
      action (num_actions - 1) g

(** Give medicine to a fish population. *)
and medicine (num_actions : int) (g : Game.game_state) : unit =
  print_endline
    "\n\
    \  Give medicine to a species in your tank to boost its health by 30 points.\n\
    \  E.g. to give medicine to the goldfish in your tank, type \n\
    \  \"Goldfish\". Medicine has a flat price of $50.";

  match read_line () |> parse_species with
  | Huh ->
      dunno ();
      action num_actions g
  | s ->
      if Game.species_extinct g s then (
        print_endline
          ("\n  You do not have any "
          ^ (s |> Game.plural_species |> String.lowercase_ascii)
          ^ " in your tank.");
        action num_actions g)
      else if Game.med_broke g then (
        print_endline "\n  You do not have enough money.";
        action num_actions g)
      else Game.med_game_species g s;
      action (num_actions - 1) g

and action num_actions (g : Game.game_state) : unit =
  try
    if num_actions = 0 then raise Exit
    else
      ANSITerminal.print_string [ ANSITerminal.black ]
        ("\n What would you like to do today? \n You have "
       ^ string_of_int num_actions ^ " action(s) left and $"
        ^ string_of_float (Game.get_playermoney g)
        ^ "  \n\
          \ Type (Buy, Feed, Medicine, Tanks, Wallet, Pass, Instructions) or \
           Ctrl +C to Exit  \n");
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
    | Pass -> action (num_actions - 1) g
    | Dunno ->
        dunno ();
        action num_actions g
  with Exit -> ()

let () =
  (* ANSITerminal.print_string [ ANSITerminal.cyan ]
     "\n\
     \     .\n\
     \    \":\"\n\
     \  ___:____     |\"\\/\"|\n\
      ,'        `.      /\n\
      |  O        \\___/  |\n\
      ~^~^~^~^~^~^~^~^~^~^~^~^~\n\n\
     \ "; *)
  ANSITerminal.print_string [ ANSITerminal.red ] "    ><>\n";
  ANSITerminal.print_string [ ANSITerminal.cyan ] "         o \n o";
  ANSITerminal.print_string [ ANSITerminal.yellow ] "          <><\n";
  ANSITerminal.print_string [ ANSITerminal.magenta ] "    ><>";
  ANSITerminal.print_string [ ANSITerminal.green ]
    "          (   \n   )       ) ";
  ANSITerminal.print_string [ ANSITerminal.cyan ] " o ";
  ANSITerminal.print_string [ ANSITerminal.green ]
    "  )\n__(_______(______(__________\n     \n";
  ANSITerminal.print_string
    [ ANSITerminal.Bold; ANSITerminal.magenta ]
    "Build your own aquarium!\n";
  ANSITerminal.print_string [ ANSITerminal.magenta ]
    "\n\
     Start the game with $100, use it to buy more fish and tanks.\n\
     Each round simulates one day and the game ends after 3 rounds.\n\n\
     Be careful: each day you have a limited amount of actions you can take! \n\n\
     Starting Game...\n";
  let game = Game.start_game 3 in
  Game.set_game game;
  for i = 1 to Game.get_max_rounds game do
    print_endline ("\n Starting Round " ^ string_of_int i);
    action 2 game;
    Game.end_of_round game
  done;
  ANSITerminal.print_string [ ANSITerminal.cyan ]
    "\n Thanks for playing! \n Here's your game summary: \n";
  Game.print_fish game
