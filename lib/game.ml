
type fish = { name : string;
  species : string;
  color : string;
  age : int;
  hunger : int }

type tank_type = Nursery | AdultTank

type tank= {tank_type : tank_type; 
fish_list : fish list; 
capacity : int }

type player_state = {
  round : int;
  money : int;
  tank : tank;
  max_rounds : int;
}

(** Given a tank type, creates a new empty tank with a capacity of 10. *)
let new_tank (t : tank_type) : tank = { 
  tank_type = t; 
  fish_list = []; 
  capacity = 10 }

(** Given a fish and tank, adds the fish to the tank's fish list and updates the remaining capacity. *)
let add_fish (f : fish) (t : tank) : tank = {
  tank_type = t.tank_type;
  fish_list = f :: t.fish_list;
  capacity = t.capacity - 1;
}

(** Given a fish and an integer i, adds i to the fish's age. *)
let age_fish (f : fish) (i : int) : fish = {
    name = f.name;
    color = f.color;
    species = f.species;
    age = f.age + i;
    hunger = f.hunger;
  }

(** Given a fish and an integer i, adds i to the fish's hunger. *)
let hunger_fish (f : fish) (i : int) : fish = {
    name = f.name;
    color = f.color;
    species = f.species;
    age = f.age;
    hunger = f.hunger + i
  }

(** Given a fish and an integer i, subtracts i from the fish's hunger. *)
let feed_fish (f : fish) (i : int) : fish = {
    name = f.name;
    color = f.color;
    species = f.species;
    age = f.age;
    hunger = f.hunger - i
  }

(** Given a list of fish, updates each the hunger of each fish by one round. *)
let rec hunger_list (lst : fish list) : fish list =
  match lst with [] -> lst | h :: t -> hunger_fish h 10 :: hunger_list t

(** Given a list of fish, feeds each fish. *)
let rec feed_list (lst : fish list) : fish list =
  match lst with [] -> lst | h :: t -> feed_fish h 5 :: feed_list t 

(** Feeds fish in tank. *)
let feed_tank (t : tank) : tank = {
  tank_type = t.tank_type;
  fish_list = feed_list t.fish_list;
  capacity = t.capacity;
}

(** Feeds fish in game with one tank. *)
let feed_game (g : player_state) : player_state = 
  { g with tank = (feed_tank g.tank ) }

(** Given a list of fish, updates each the age of each fish by one round. *)
let rec age_list (lst : fish list) : fish list =
  match lst with [] -> lst | h :: t -> age_fish h 1 :: age_list t

 (** Given a tank, updates the age of every fish in the tank by one round. *)
 let update_tank_ages (t : tank) : tank =
  {
    tank_type = t.tank_type;
    fish_list = age_list t.fish_list;
    capacity = t.capacity;
  }

(** Starts a new game instance with i rounds. A player starts with $100 and one nursery tank. *)
let start_game (i : int) : player_state = {
  round = 1;
  money = 100;
  tank = new_tank Nursery;
  max_rounds = i
}

(** Given a player state and a fish, adds the fish to the player state's tank. *)
let add_fish_game (g : player_state) (f : fish) : player_state = {
  round = g.round;
  money = g.money;
  tank = add_fish f g.tank;
  max_rounds = g.max_rounds
}

(** Updates player state's round number and tank ages after one round. *)
let end_of_round (g:player_state) : player_state = {
  round = g.round + 1;
  money = g.money;
  tank = update_tank_ages g.tank;
  max_rounds = g.max_rounds
}

(** Prints a fish's name, speies, and age. Helper function for print_fish_list. *)
let fish_bio (f : fish) : string = "  "^(f.name) ^ "       " ^ (f.species)^ 
  "       "^(string_of_int f.age)^"       "^(string_of_int f.hunger)

(**Prints a fish_list using fish_bio. Helper function for print_tank*)
let rec print_fish_list (lst:fish list) : string = 
  match lst with
  | [] -> ""
  | h :: t -> fish_bio h ^"\n"^ print_fish_list t

let print_tank (t:tank) : string = 
  "\n Tank Contents: \n Name:       Species:      Age:      Hunger Level: \n"
  ^ print_fish_list (List.rev t.fish_list )

let print_playermoney (g:player_state):int = g.money

(** Initialize a round by printing the current round and currency. *)
let start_round_print (g:player_state) : string = 
  "Day " ^(string_of_int g.round)^"\nYou currently have $" ^ (string_of_int (g.money)) ^ "."

(** End of round string with a players fish in a table. *)
let end_round_print (g:player_state) : string = print_tank g.tank
let cost (g : player_state) (cost : int) : player_state = {
  round = g.round;
  money = g.money - cost;
  tank = g.tank;
  max_rounds = g.max_rounds
}
let make_fish (n:string) (sp:string) (color:string): fish = {
  name = n;
  species = sp;
  color = color;
  age = 0;
  hunger = 20;
}

let goldfish (n:string) : fish = make_fish n "Goldfish" "orange"
let pufferfish (n:string) : fish = make_fish n "Pufferfish" "yellow"
let shark (n:string) : fish = make_fish n "Shark" "grey"