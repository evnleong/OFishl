type fish_species = Goldfish | Pufferfish | Shark | Huh

let string_of_fish_species (s : fish_species) : string =
  match s with
  | Goldfish -> "Goldfish"
  | Pufferfish -> "Pufferfish"
  | Shark -> "Shark"
  | Huh -> "Huh"

let plural_species (s : fish_species) : string =
  match s with
  | Goldfish -> "Goldfish"
  | Pufferfish -> "Pufferfish"
  | Shark -> "Sharks"
  | Huh -> "Huh"

(** Type representing fish foods. *)
type fish_food = Fish | Pellet

type fish = {
  species : fish_species;
  mutable num : int;
  mutable age_sum : int;
  mutable health : float;
  food : fish_food;
  growth_rate : float;
  death_rate : float;
}
(** Type representing a population of fish. *)

(* Total number of fish species excluding Huh. *)
let num_species = 3

(* Amount by which health of a fish population decreases each round. *)
let health_points = ~-.5.

(* Amount by which health a fish population increases with medicine. *)
let med_boost = 30.

(* Cost of a medicine boost. *)
let med_cost = 50.

(* Cost of a single food pellet. *)
let pellet_cost = 0.1

type tank = fish array
(** Type representing a tank of fish. *)

type game_state = {
  mutable round : int;
  mutable money : float;
  mutable tank : tank;
  max_rounds : int;
}
(** Type representing current game state. *)

(** Creates a custom new fish population. *)
let make_fish (species : fish_species) (food : fish_food) (growth_rate : float)
  (death_rate : float) : fish =
  { species; num = 0; age_sum = 0; health = 100.; food; growth_rate; 
    death_rate}

(** Cost of buying n fish of a species *)
let price_fish (species : fish_species) (n : int) : float =
  let m = float_of_int n in
  match species with
  | Goldfish -> 5. *. m
  | Pufferfish -> 10. *. m
  | Shark -> 20. *. m
  | Huh -> failwith "Invalid"

(** Sets tank t to the empty tank. *)
let set_tank (t : tank) : unit =
  t.(0) <- make_fish Goldfish Pellet 1.3 0.6;
  t.(1) <- make_fish Pufferfish Pellet 1.2 0.7;
  t.(2) <- make_fish Shark Fish 1.1 0.9

(* Returns position of fish population of species s in tank array. *)
let fish_pos (s : fish_species) : int =
  match s with
  | Goldfish -> 0
  | Pufferfish -> 1
  | Shark -> 2
  | Huh -> failwith "Invalid"

(** Initializes a new game state. *)
let start_game (i : int) : game_state =
  {
    round = 1;
    money = 100.;
    tank = Array.make num_species (make_fish Goldfish Pellet 1.3 0.6);
    max_rounds = i;
  }

(** Sets tank in game state g to the empty tank. *)
let set_game (g : game_state) : unit = set_tank g.tank

(** Checks whether a fish popuation is extinct. *)
let get_extinct (f : fish) : bool = (f.num = 0)

(** Checks whether species s is extinct in game g. *)
let species_extinct (g : game_state) (s : fish_species) : bool = 
  let pos = fish_pos s in get_extinct g.tank.(pos)

(** Subtracts cost from game state g's money. *)
let cost (g : game_state) (c : float) : unit = g.money <- g.money -. c

(** Adds n to fish population f. *)
let add_fish (f : fish) (n : int) : unit = f.num <- f.num + n

(** Adds n to fish population of species s in tank t. *)
let add_fish_tank (t : tank) (s : fish_species) (n : int) : unit =
  let pos = fish_pos s in
  add_fish t.(pos) n

(** Checks if player does not have enough money to buy fish. *)
let buy_broke (g : game_state) (s : fish_species) (n : int) : bool = 
  price_fish s n > g.money 

(** Adds n fish to fish population of species s in game state g. Subtracts
    cost of the fish from g's money. *)
let buy_fish_game (g : game_state) (s : fish_species) (n : int) : unit =
  add_fish_tank g.tank s n;
  cost g (price_fish s n)

(** Ages a fish population f by one round. In effect, f's age sum increases
    by the number of fish in f. *)
let age_fish (f : fish) : unit = f.age_sum <- f.age_sum + f.num

(** Ages every fish population in a tank by one round. *)
let age_tank (t : tank) : unit =
  for x = 0 to num_species - 1 do
    if t.(x).num > 0 then age_fish t.(x)
  done

(** Adds i to the health of fish population f. *)
let health_fish (f : fish) (i : float) : unit = f.health <- f.health +. i

(** Adds i to the health of fish population of species s in tank t. *)
let health_tank_species (t : tank) (s : fish_species) (i : float) : unit =
  let pos = fish_pos s in
  health_fish t.(pos) i

(** Adds i to the health of every fish population in tank t. *)
let health_tank (t : tank) (i : float) : unit =
  for x = 0 to num_species - 1 do
    if t.(x).num > 0 then health_fish t.(x) i
  done

(** Checks if player does not have enough money to buy medicine. *)
let med_broke (g : game_state) : bool = 
  med_cost > g.money 

(** Feeds medicine to fish population of species s in game g. In effect, 
    the health of the population gets boosted by med_boost, and med_cost
    is subtracted from g's money. *)
let med_game_species (g : game_state) (s : fish_species) : unit =
  health_tank_species g.tank s med_boost;
  cost g med_cost

(** Feeds n pellets to a fish population f. In effect, f's health increases 
    by n/N, where N is the number of fish in the population. *)
let feed_fish (f : fish) (n : int) : unit =
  health_fish f (float_of_int n /. float_of_int f.num)

(** Feeds n pellets to fish population of species s in tank t. *)
let feed_fish_tank (t : tank) (s : fish_species) (n : int) : unit =
  let pos = fish_pos s in
  feed_fish t.(pos) n

(** Feeds n pellets to fish population of species s in game state g. Subtracts
    cost of fish from g's money. *)
let feed_fish_game (g : game_state) (s : fish_species) (n : int) : unit =
  let m = float_of_int n in
  feed_fish_tank g.tank s n;
  cost g (m *. pellet_cost)

(** Checks if player has enough money to feed fish. *)
let feed_broke (g : game_state) (n : int) : bool = 
  let m = float_of_int n in
  g.money < (m *. pellet_cost)

(** Checks if a fish is a predator. *)
let predator_fish (f : fish) : bool = (f.food = Fish)

(** Checks if species s in game g is a predator. *)
let predator_species (g : game_state) (s : fish_species) : bool = 
  let pos = fish_pos s in 
  predator_fish g.tank.(pos) 

let earnings (g : game_state) : float =
  (float_of_int g.tank.(0).num *. 1.)
  +. (float_of_int g.tank.(1).num *. 5.)
  +. (float_of_int g.tank.(2).num *. 10.)

(** Remind the player when a species needs to be fed. *)
let health_reminder (g : game_state) : unit =
  for i = 0 to num_species - 1 do
    if g.tank.(i).health < 20. then
      print_endline
        ("Your" 
        ^ (g.tank.(i).species |> string_of_fish_species |> String.lowercase_ascii)
        ^ "are hungry!")
    else ()
  done

(** Updates count of a fish population. *)
let growth_fish (f : fish) : unit = 
  if f.health <= 0. || f.num <= 0 then (
    print_endline ("Your " ^ (f.species |> plural_species |> String.lowercase_ascii) 
      ^ " have gone extinct.");
    f.num <- 0;
    f.health <- 100.)
  else if f.health < 50. then 
    f.num <- (float_of_int f.num) *. f.death_rate |> Float.to_int
  else if f.health > 80. then 
    f.num <- (float_of_int f.num) *. f.growth_rate |> Float.to_int
  else ()

(** Updates count of each fish population in a tank. *)
let growth_tank (t : tank) : unit = 
  for i = 0 to num_species - 1 do 
    if t.(i).num > 0 then growth_fish t.(i)
  done

(** Updates game state g's round, fish population ages by one round. *)
let end_of_round (g : game_state) : unit =
  g.round <- g.round + 1;
  g.money <- g.money +. earnings g;
  print_endline 
    ("\n  You earned $" ^ string_of_float (earnings g) ^ " today." 
    ^ "\n  Next day....");
  age_tank g.tank;
  if g.round > 0 then growth_tank g.tank;
  health_tank g.tank health_points;
  health_reminder g

(* CHANGE IMPLEMENTATION OF FUNCTIONS BELOW *)

(** Summarizes the health of a player's fish. *)
let health_statement (g : game_state) =
  let playertank = g.tank in
  let body = ref "" in
  let header = ref "" in
  for i = 0 to num_species - 1 do
    body := !body ^ "           " ^ string_of_float playertank.(i).health;
    header := !header ^ "      " ^ string_of_fish_species playertank.(i).species
  done;
  print_endline ("Health:       " ^ !body)

(** Returns 0 if f has no fish, otherwise returns f's health. *)
let string_of_health (f : fish) : string = 
  if f.num = 0 then "0" else string_of_float f.health 

let print_fish (pstate : game_state) =
  let playertank = pstate.tank in
  let body1 = ref "" in
  let body2 = ref "" in
  let header = ref "" in
  for i = 0 to num_species - 1 do
    body1 := !body1 ^ "             " ^ string_of_int playertank.(i).num;
    body2 := !body2 ^ "            " ^ string_of_health playertank.(i);
    header := !header ^ "      " ^ string_of_fish_species playertank.(i).species
  done;

  print_endline ("\n Species: " ^ !header);
  print_endline (" Count:" ^ !body1);
  print_endline (" Health:" ^ !body2)

(* pstate.tank |> Array.iter (fun f -> print_string (string_of_int f.num)) *)
(*
   f.species ^ "       " ^ string_of_int f.num ^ "       "
   ^ string_of_int f.age_sum ^ "       " ^ string_of_int f.health *)

(** Prints a fish_list using fish_bio. Helper function for print_tank *)
(* let rec print_fish_list (lst : fish list) : string =
   match lst with [] -> "" | h :: t -> print_fish h ^ "\n" ^ print_fish_list t *)

(* let print_tank (t : tank) : string =
   "\n\
   \ Tank Contents: \n\
   \ Species:      # of Fish:      Age Score:      Health Level: \n"
   ^ print_fish_list (List.rev t.fish_list) *)

let get_playermoney (g : game_state) : float = g.money
let get_max_rounds (g : game_state) : int = g.max_rounds

(** Initialize a round by printing the current round and currency. *)
let start_round_print (g : game_state) : string =
  "Day " ^ string_of_int g.round ^ "\nYou currently have $"
  ^ string_of_float g.money ^ "."

(** End of round string with a players fish in a table. *)
(* let end_round_print (g : game_state) : string = print_tank g.tank *)
