(* Opening Modules in lib. *)
open Final_project
open Userinput

(** Message for erroneous user inputs. *)
let dunno () = print_endline "\n  Oops, we didn't catch that."

(** Buy fish. *)
let rec buy (num_actions : int) (g : Game.game_state) : unit =
  ANSITerminal.print_string [ ANSITerminal.cyan ]
    (*"\n  You currently have $"
      ^ string_of_float (Game.get_playermoney g) *)
    "\n\
    \  Buy fish by typing a species name and a number, ex. \"Goldfish 10\".\n\
    \  Price per fish: Goldfish $2, Anemone $4, Remora $8, Clownfish $10, \n\
    \  Turtle $15, Shark $20.\n";

  match parse_species_int (read_line ()) with
  | s, n ->
      if s = Huh then (
        dunno ();
        action num_actions g)
      else if Game.buy_broke g s n then (
        print_endline "\n  You do not have enough money.";
        action num_actions g)
      else if n < 0 then (
        print_endline "\n  You must enter a positive integer";
        action num_actions g)
      else (
        Game.buy_fish_game g s n;
        action (num_actions - 1) g)

(** Feed n pellets to fish population. *)
and feed (num_actions : int) (g : Game.game_state) : unit =
  ANSITerminal.print_string [ ANSITerminal.cyan ]
    "\n\
    \  Feed pellets to a species by typing its name and a number, ex. \n\
    \  \"Goldfish 10\". One pellet costs $0.1. If a species has N fish, \n\
    \  then feeding it n pellets increases its health by n/N.\n";

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
      else (
        Game.feed_fish_game g s n;
        action (num_actions - 1) g)

(** Give medicine to a fish population. *)
and medicine (num_actions : int) (g : Game.game_state) : unit =
  ANSITerminal.print_string [ ANSITerminal.cyan ]
    "\n\
    \  Boost the health of a species by 30 points for $50.\n\
    \  Type a species name, ex. \"Goldfish\".\n";

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
      else (
        Game.med_game_species g s;
        action (num_actions - 1) g)

and action num_actions (g : Game.game_state) : unit =
  try
    if num_actions = 0 then raise Exit
    else
      ANSITerminal.print_string [ ANSITerminal.default ]
        ("\n  What would you like to do today? \n  You have $"
        ^ string_of_float (Game.get_playermoney g)
        ^ " and " ^ string_of_int num_actions ^ " action(s) left."
        ^ "  \n\
          \  Type (Buy, Feed, Medicine, Tank, Pass, Manual) or Ctrl +C to Exit  \n"
        );
    let response = parse_input (read_line ()) in
    match response with
    | Buy -> buy num_actions g
    | Feed -> feed num_actions g
    | Medicine -> medicine num_actions g
    | View_Tanks ->
        Game.print_fish g;
        action num_actions g
    | Manual -> manual num_actions g
    | Pass -> action (num_actions - 1) g
    | Dunno ->
        dunno ();
        action num_actions g
  with Exit -> ()

(** Displays additional information about the game. *)
and manual (num_actions : int) (g : Game.game_state) : unit =
  ANSITerminal.print_string
    [ ANSITerminal.Bold; ANSITerminal.blue ]
    "\n  Additional Information: \n";
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "\n\
    \  1. The health of every species except sharks will automatically decrease \n\
    \     by 5 points at the end of a round (see 3). \n\
    \  2. A species will increase in health when given food or medicine. \n\
    \  3. Sharks eat fish to survive. The only fish they do not eat are remorae \n\
    \     and other sharks. At the end of a round, every shark in your tank \
     will \n\
    \     randomly devour some other fish. For every shark that has no fish to \n\
    \     eat, the health of sharks will decrease by 2 points.\n\
    \  4. There are 2 symbiotic pairings: clownfish and anemone; sharks and \
     remorae.\n\
    \     If both species of a pairing are present in your tank at the end of a \n\
    \     round, then they will give each other a small health boost.\n\
    \  5. A species with health > 80 and enough fish will grow on its own.\n\
    \     If a species has health < 50, then its number will automatically \
     decline.\n\
    \  6. At the end of a round, you will earn an amount of money that depends \
     on the\n\
    \     number of each species in your tank.\n\
     7. Keep an eye out for random events and announcements that can occur in \
     between rounds! \n\
    \     \n\
    \      8. At the end of the game, you will receive a score that depends on \
     the\n\
    \     cumulative ages of the species in your tank.\n";
  action num_actions g

let () =
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
    "Build Your Own Aquarium!\n";
  ANSITerminal.print_string [ ANSITerminal.magenta ]
    "===============================================================";
  ANSITerminal.print_string [ ANSITerminal.magenta ]
    "\n\
     Start the game with $100, use it to buy and feed fish.\n\
     Each round simulates one day and the game ends after 5 rounds.\n\n\
     Be careful: Each day, you have a limited number of actions! \n\n\
     Starting Game...\n";
  let game = Game.start_game 5 in
  Game.set_game game;
  while not (Game.game_ended game) do
    ANSITerminal.print_string
      [ ANSITerminal.Bold; ANSITerminal.black ]
      ("\n  Round " ^ (game |> Game.get_round |> string_of_int) ^ "\n");
    action 2 game;
    Game.end_of_round game
  done;
  ANSITerminal.print_string
    [ ANSITerminal.Bold; ANSITerminal.black ]
    ("\n  END OF GAME. YOU SCORED "
    ^ string_of_int (Game.end_score game)
    ^ " POINTS.\n");
  if Game.end_score game > 2000 then (
    ANSITerminal.print_string [ ANSITerminal.yellow ]
      "=================================";
    ANSITerminal.print_string [ ANSITerminal.yellow ]
      " Acheivement Unlocked: Aquarium Master ";
    ANSITerminal.print_string [ ANSITerminal.yellow ]
      "=================================")
  else if Game.end_score game > 1500 then (
    ANSITerminal.print_string [ ANSITerminal.yellow ]
      "=================================";
    ANSITerminal.print_string [ ANSITerminal.yellow ]
      " Acheivement Unlocked: Shark Swimmer ";
    ANSITerminal.print_string [ ANSITerminal.yellow ]
      "=================================")
  else if Game.end_score game > 1000 then (
    ANSITerminal.print_string [ ANSITerminal.yellow ]
      "=================================";
    ANSITerminal.print_string [ ANSITerminal.yellow ]
      " Acheivement Unlocked: Fish Feeder ";
    ANSITerminal.print_string [ ANSITerminal.yellow ]
      "=================================")
  else (
    ANSITerminal.print_string [ ANSITerminal.yellow ]
      "=================================";
    ANSITerminal.print_string [ ANSITerminal.yellow ]
      " Acheivement Unlocked: Kept Typing Pass ";
    ANSITerminal.print_string [ ANSITerminal.yellow ]
      "=================================");
  ANSITerminal.print_string [ ANSITerminal.cyan ]
    "\n  Thanks for playing! Here's your game summary: \n";
  Game.print_fish game
