type fish = { species : string; num : int; age_sum : int; health : int }
(**Type representing a fish species, 
    contains fields for the species name, how many, the collective age and health*)

type tank = { fish_list : fish list }
type player_state = { round : int; money : int; tank : tank; max_rounds : int }

(** Given a fish list, creates a new tank containing those fish. *)
let new_tank (f : fish list) : tank = { fish_list = f }

(** Given a tank, species, and an integer i,  adds i fish of that species to the 
     tank's fish list. UNIMPLEMENTED*)
let add_fish (f : fish) (t : tank) : tank = { fish_list = f :: t.fish_list }

(** Given a fish, updates the fish's age sum *)
let age_fish (f : fish) : fish =
  {
    species = f.species;
    num = f.num;
    age_sum = f.age_sum + f.num;
    health = f.health;
  }

(** Given a list of fish, updates each the age of each species by one round. *)
let rec age_list (lst : fish list) : fish list =
  match lst with [] -> lst | h :: t -> age_fish h :: age_list t

(** Given a tank, updates the age of every fish in the tank by one round. *)
let age_tank (t : tank) : tank = { fish_list = age_list t.fish_list }

(** Given a fish and an integer i, adds i to the fish's health. *)
let health_fish (f : fish) (i : int) : fish =
  {
    species = f.species;
    num = f.num;
    age_sum = f.age_sum;
    health = f.health + i;
  }

(** Given a list of fish, updates each the health of each species by one round. *)
let rec health_list (lst : fish list) (i : int) : fish list =
  match lst with [] -> lst | h :: t -> health_fish h i :: health_list t i

(** Updates the health of a fish in a tank *)
let health_tank (t : tank) (i : int) : tank =
  { fish_list = health_list t.fish_list i }

(** Starts a new game instance with i rounds. A player starts with $100 and one nursery tank. *)
let start_game (i : int) : player_state =
  { round = 1; money = 100; tank = new_tank []; max_rounds = i }

(** Given a player state and a fish, adds the fish to the player state's tank. *)
let add_fish_game (g : player_state) (f : fish) : player_state =
  {
    round = g.round;
    money = g.money;
    tank = add_fish f g.tank;
    max_rounds = g.max_rounds;
  }

(** Updates player state's round number and tank ages after one round. *)
let end_of_round (g : player_state) : player_state =
  {
    round = g.round + 1;
    money = g.money;
    tank = age_tank g.tank;
    max_rounds = g.max_rounds;
  }

(** Prints a fish's name, speies, and age. Helper function for print_fish_list. *)
let print_fish (f : fish) : string =
  f.species ^ "       " ^ string_of_int f.num ^ "       "
  ^ string_of_int f.age_sum ^ "       " ^ string_of_int f.health

(**Prints a fish_list using fish_bio. Helper function for print_tank*)
let rec print_fish_list (lst : fish list) : string =
  match lst with [] -> "" | h :: t -> print_fish h ^ "\n" ^ print_fish_list t

let print_tank (t : tank) : string =
  "\n\
  \ Tank Contents: \n\
  \ Species:      # of Fish:      Age Score:      Health Level: \n"
  ^ print_fish_list (List.rev t.fish_list)

let print_playermoney (g : player_state) : int = g.money

(** Initialize a round by printing the current round and currency. *)
let start_round_print (g : player_state) : string =
  "Day " ^ string_of_int g.round ^ "\nYou currently have $"
  ^ string_of_int g.money ^ "."

(** End of round string with a players fish in a table. *)
let end_round_print (g : player_state) : string = print_tank g.tank

let cost (g : player_state) (cost : int) : player_state =
  {
    round = g.round;
    money = g.money - cost;
    tank = g.tank;
    max_rounds = g.max_rounds;
  }
