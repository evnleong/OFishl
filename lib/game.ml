type fish_species =
  | Goldfish
  | Anemone
  | Clownfish
  | Turtle
  | Remora
  | Shark
  | Huh

let string_of_species (s : fish_species) : string =
  match s with
  | Goldfish -> "Goldfish"
  | Anemone -> "Anemone"
  | Clownfish -> "Clownfish"
  | Turtle -> "Turtle"
  | Remora -> "Remora"
  | Shark -> "Shark"
  | Huh -> "Huh"

let plural_species (s : fish_species) : string =
  match s with
  | Goldfish -> "Goldfish"
  | Anemone -> "Anemones"
  | Clownfish -> "Clownfish"
  | Turtle -> "Turtles"
  | Remora -> "Remorae"
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
  mutable extinct : bool;
}
(** Type representing a population of fish. *)

(* Total number of fish species excluding Huh. *)
let num_species = 6

(* Cost of a single food pellet. *)
let pellet_cost = 0.1

(* Type representing a tank of fish. *)
type tank = fish array

type game_state = {
  mutable round : int;
  mutable money : float;
  mutable tank : tank;
  max_rounds : int;
  mutable ended : bool;
}
(** Type representing current game state. *)

(** Creates a custom empty fish population. *)
let make_fish (species : fish_species) (food : fish_food) (growth_rate : float)
    (death_rate : float) : fish =
  {
    species;
    num = 0;
    age_sum = 0;
    health = 100.;
    food;
    growth_rate;
    death_rate;
    extinct = true;
  }

(** Cost of buying n fish of a species. *)
let price_fish (species : fish_species) (n : int) : float =
  let m = float_of_int n in
  match species with
  | Goldfish -> 2. *. m
  | Anemone -> 4. *. m
  | Clownfish -> 10. *. m
  | Turtle -> 15. *. m
  | Remora -> 8. *. m
  | Shark -> 20. *. m
  | Huh -> failwith "Invalid"

(* Returns position of fish population of species s in tank array. *)
let fish_pos (s : fish_species) : int =
  match s with
  | Goldfish -> 0
  | Anemone -> 1
  | Clownfish -> 2
  | Turtle -> 3
  | Remora -> 4
  | Shark -> 5
  | Huh -> failwith "Invalid"

(* Returns species in position [pos] of tank [t]. *)
let pos_to_species (pos : int) : fish_species =
  match pos with
  | 0 -> Goldfish
  | 1 -> Anemone
  | 2 -> Clownfish
  | 3 -> Turtle
  | 4 -> Remora
  | 5 -> Shark
  | _ -> Huh

(** Sets tank t to the empty tank. *)
let set_tank (t : tank) : unit =
  t.(fish_pos Goldfish) <- make_fish Goldfish Pellet 1.4 0.6;
  t.(fish_pos Anemone) <- make_fish Anemone Pellet 1.3 0.7;
  t.(fish_pos Clownfish) <- make_fish Clownfish Pellet 1.3 0.7;
  t.(fish_pos Turtle) <- make_fish Turtle Pellet 1.2 0.9;
  t.(fish_pos Remora) <- make_fish Remora Pellet 1.2 0.8;
  t.(fish_pos Shark) <- make_fish Shark Fish 1.1 0.9

(** Initializes a new game state. *)
let start_game (i : int) : game_state =
  {
    round = 1;
    money = 100.;
    tank = Array.make num_species (make_fish Goldfish Pellet 1.4 0.6);
    max_rounds = i;
    ended = false;
  }

(** Sets tank in game state g to the empty tank. *)
let set_game (g : game_state) : unit = set_tank g.tank

(** Checks whether a fish popuation is extinct. *)
let extinct (f : fish) : bool = f.extinct

(** Checks whether species s in game g is extinct. *)
let species_extinct (g : game_state) (s : fish_species) : bool =
  let pos = fish_pos s in
  extinct g.tank.(pos)

(** Subtracts cost from game state g's money. *)
let cost (g : game_state) (c : float) : unit = g.money <- g.money -. c

(** Adds i to the health of fish population f. *)
let health_fish (f : fish) (i : float) : unit =
  let new_health = f.health +. i in
  if new_health > 100. then f.health <- 100. else f.health <- new_health

(** Adds i to the health of fish population of species s in tank t. *)
let health_tank_species (t : tank) (s : fish_species) (i : float) : unit =
  let pos = fish_pos s in
  health_fish t.(pos) i

(** Adds i to the health of every fish population in tank t. *)
let health_tank (t : tank) (i : float) : unit =
  for x = 0 to 4 do
    if t.(x).num > 0 then health_fish t.(x) i
  done

(** Adds n to fish population f. *)
let add_fish (f : fish) (n : int) : unit = f.num <- f.num + n

(** Adds n to fish population of species s in tank t. *)
let add_fish_tank (t : tank) (s : fish_species) (n : int) : unit =
  let pos = fish_pos s in
  if extinct t.(pos) then (
    add_fish t.(pos) n;
    t.(pos).extinct <- false;
    health_fish t.(pos) 100.)
  else add_fish t.(pos) n

(** Checks if player does not have enough money to buy fish. *)
let buy_broke (g : game_state) (s : fish_species) (n : int) : bool =
  price_fish s n > g.money

(** Adds n fish to fish population of species s in game state g. Subtracts
    cost of the fish from g's money. *)
let buy_fish_game (g : game_state) (s : fish_species) (n : int) : unit =
  add_fish_tank g.tank s n;
  cost g (price_fish s n)

(* Returns list of nonextinct prey species in tank [t], where prey
    includes every species fish except shark and remora. Each
    species is represented by its position in the tank array. *)
let prey_lst (t : tank) : int list =
  let lst = ref [] in
  for pos = 0 to 3 do
    if not (extinct t.(pos)) then lst := pos :: !lst
  done;
  !lst

(** Removes [n] fish of species in position [pos] of tank [t]. *)
let eat_fish_species (t : tank) (pos : int) (n : int) : unit =
  t.(pos).num <- t.(pos).num - n;
  if t.(pos).num = 0 then t.(pos).extinct <- true

(* Randomly removes a fish of some nonextinct species in the tank
   and returns species attacked. If all prey species extinct,
   decreases sharks' health and returns [Huh]. *)
let shark_bite (t : tank) : fish_species =
  let lst = prey_lst t in
  if lst = [] then (
    health_tank_species t Shark ~-.5.;
    Huh)
  else
    let n = List.length lst in
    let prey_pos = List.nth lst (Random.int n) in
    eat_fish_species t prey_pos 1;
    pos_to_species prey_pos

(* Each shark eats one fish, otherwise shark health decreases.
    Returns array tracking the number of each species eaten. *)
let shark_dinner (t : tank) : int array =
  let num_sharks = t.(5).num in
  let track = Array.make 4 0 in
  (* Track the fish eaten. Position of species in [track]
     corresponds to position in [g.tank]. *)
  for x = 1 to num_sharks do
    ignore x;
    let species = shark_bite t in
    if species <> Huh then
      track.(fish_pos species) <- track.(fish_pos species) + 1
  done;
  track

(** Converts int array into string list. *)
let shark_news (track : int array) : string =
  let lst = ref "" in
  for pos = 3 downto 0 do
    if track.(pos) = 1 then
      lst :=
        !lst ^ "\n  1 "
        ^ (pos |> pos_to_species |> string_of_species |> String.lowercase_ascii)
    else if track.(pos) <> 0 then
      lst :=
        !lst ^ "\n  "
        ^ string_of_int track.(pos)
        ^ " "
        ^ (pos |> pos_to_species |> plural_species |> String.lowercase_ascii)
  done;
  "\n  Your sharks ate the following fish today:" ^ !lst ^ ". \n"
[@@coverage off]

(** Feeds shark in tank at end of round and prints message with
    the fish eaten. *)
let shark_update (g : game_state) : unit =
  let t = g.tank in
  let track = shark_dinner t in
  if Array.find_opt (fun x -> x > 0) track <> None then
    print_string (shark_news track)
  else print_string "\n  Your sharks had nothing to eat this round."
[@@coverage off]

(** Ages a fish population f by one round. In effect, f's age sum increases
    by the number of fish in f. *)
let age_fish (f : fish) : unit = f.age_sum <- f.age_sum + f.num

(** Ages every fish population in a tank by one round. *)
let age_tank (t : tank) : unit =
  for x = 0 to num_species - 1 do
    if t.(x).num > 0 then age_fish t.(x)
  done

(** Checks if player does not have enough money to buy medicine. *)
let med_broke (g : game_state) : bool = 50. > g.money

(** Feeds medicine to fish population of species s in game g. In effect, 
    the health of the population gets boosted by 30, and 50
    is subtracted from g's money. *)
let med_game_species (g : game_state) (s : fish_species) : unit =
  health_tank_species g.tank s 30.;
  cost g 50.

(* Helper for feed_fish that rounds float to 1 dp *)
let round_float (f : float) : float = (f *. 10. |> Float.round) /. 10.

(** Feeds n pellets to a fish population f. In effect, f's health increases 
    by n/N, where N is the number of fish in the population. *)
let feed_fish (f : fish) (n : int) : unit =
  health_fish f (float_of_int n /. float_of_int f.num |> round_float)

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
  g.money < m *. pellet_cost

(** Checks if a fish is a predator. *)
let predator_fish (f : fish) : bool = f.food = Fish

(** Checks if species s in game g is a predator. *)
let predator_species (g : game_state) (s : fish_species) : bool =
  let pos = fish_pos s in
  predator_fish g.tank.(pos)

(** Computes daily earnings. *)
let earnings (g : game_state) : float =
  (float_of_int g.tank.(fish_pos Goldfish).num *. 1.)
  +. (float_of_int g.tank.(fish_pos Anemone).num *. 2.)
  +. (float_of_int g.tank.(fish_pos Clownfish).num *. 3.)
  +. (float_of_int g.tank.(fish_pos Turtle).num *. 6.)
  +. (float_of_int g.tank.(fish_pos Remora).num *. 1.)
  +. (float_of_int g.tank.(fish_pos Shark).num *. 8.)

(** Returns end of game score *)
let end_score (g : game_state) : int =
  let raw_score =
    (float_of_int g.tank.(fish_pos Goldfish).age_sum *. 2.)
    +. (float_of_int g.tank.(fish_pos Anemone).age_sum *. 4.)
    +. (float_of_int g.tank.(fish_pos Clownfish).age_sum *. 10.)
    +. (float_of_int g.tank.(fish_pos Turtle).age_sum *. 15.)
    +. (float_of_int g.tank.(fish_pos Remora).age_sum *. 8.)
    +. (float_of_int g.tank.(fish_pos Shark).age_sum *. 20.)
    +. (g.money *. 10.)
  in
  int_of_float raw_score

(** Remind the player when a species needs to be fed. *)
let health_reminder (g : game_state) : unit =
  for i = 0 to num_species - 1 do
    if g.tank.(i).health < 20. && not (extinct g.tank.(i)) then
      print_endline
        ("\n  Your "
        ^ (g.tank.(i).species |> plural_species |> String.lowercase_ascii)
        ^ " are hungry!")
    else ()
  done
[@@coverage off]

(** Updates count of a fish population. *)
let growth_fish (f : fish) : unit =
  if f.health < 50. then
    f.num <- float_of_int f.num *. f.death_rate |> Float.floor |> Float.to_int
  else if f.health > 80. then
    f.num <- float_of_int f.num *. f.growth_rate |> Float.round |> Float.to_int

(** Updates count of each fish population in a tank. *)
let growth_tank (t : tank) : unit =
  for i = 0 to num_species - 1 do
    if t.(i).num > 0 then growth_fish t.(i)
  done

let check_extinct (t : tank) : unit =
  for i = 0 to num_species - 1 do
    let f = t.(i) in
    if (not f.extinct) && (f.health <= 0. || f.num <= 0) then (
      print_endline
        ("\n  Your "
        ^ (f.species |> plural_species |> String.lowercase_ascii)
        ^ " have gone extinct.");
      f.num <- 0;
      f.health <- 100.;
      f.extinct <- true)
  done

(** Gives health boost to anemone and clownfish populations in tank t if 
    both  are not empty. *)
let anemone_clownfish (t : tank) : unit =
  let a = fish_pos Anemone in
  let c = fish_pos Clownfish in

  match (extinct t.(a), extinct t.(c)) with
  | false, false ->
      health_fish t.(a) 2.;
      health_fish t.(c) 2.
  | _ -> ()

(** Gives health boost to shark and remora populations in tank t if 
    both are not empty. *)
let remora_shark (t : tank) : unit =
  let r = fish_pos Remora in
  let s = fish_pos Shark in

  match (extinct t.(r), extinct t.(s)) with
  | false, false ->
      health_fish t.(r) 2.;
      health_fish t.(s) 2.
  | _ -> ()

(** Gives health boost to symbiotic fish species in tank t. 
    Symbiotic pairings: anemone and clownfish; remora and shark. *)
let symbiosis (t : tank) : unit =
  anemone_clownfish t;
  remora_shark t

(**Player will have a random chance of losing money in between rounds. Chance increases based on number of fish player owns*)
let randomly_lose_money (g : game_state) =
  Random.self_init ();
  let loss = 30. -. float_of_int (Random.int 15) in
  let totalfishnum =
    Array.fold_left (fun acc fish -> acc + fish.num) 0 g.tank
  in
  if Random.int (min ((totalfishnum / 10) + 1) 3) = 0 then (
    g.money <- g.money -. loss;
    print_endline
      ("\n Ticket sales were down this round! You lost " ^ string_of_float loss
     ^ " dollars."))

let randomly_get_money (g : game_state) =
  Random.self_init ();
  let gain = 20. -. float_of_int (Random.int 15) in
  if Random.int 4 = 0 then (
    g.money <- g.money +. gain;
    print_endline
      ("\n\
       \ Congrats! Your aquarium had booming ticket sales this round! You \
        gained an extra " ^ string_of_float gain ^ " dollars."))

let activity_check (g : game_state) : unit =
  let totalfishnum =
    Array.fold_left (fun acc fish -> acc + fish.num) 0 g.tank
  in
  if totalfishnum < 20 then
    print_endline
      "\n\
      \  Attention: Your aquarium requires maintenance! Without new fish, \
       there's an increased risk of reduced ticket sales.";
  if not (buy_broke g Goldfish 1) then (
    print_endline
      "  Consider using your money to buy new fish to keep your aquarium \
       vibrant and attractive!";
    randomly_get_money g)
  else
    print_endline
      "  Unfortunately, you don't have enough money to buy new fish. Be \
       cautious!"

let celebrity_vist (g : game_state) =
  Random.self_init ();
  let gain = 20. -. float_of_int (Random.int 15) in
  let rarefishnum =
    List.fold_left
      (fun acc species -> acc + g.tank.(fish_pos species).num)
      0
      [ Shark; Turtle; Clownfish ]
  in
  if rarefishnum > 2 && Random.int 3 = 0 then (
    g.money <- g.money +. gain;
    print_endline
      ("\n\
       \  Woah! A celebrity visited your aquarium and liked your expensive \
        fish -- you got a stellar review!\n\
       \        \n\
       \ You gained an extra " ^ string_of_float gain ^ " dollars."))

let game_ended (g : game_state) : bool = g.ended

(** Updates game state g's round, fish population ages by one round. *)
let end_of_round (g : game_state) : unit =
  if not (extinct g.tank.(5)) then shark_update g;
  print_endline "\n \n  While you were gone...";
  activity_check g;
  celebrity_vist g;
  Random.self_init ();
  if Random.int 2 = 1 then randomly_lose_money g
  else if Random.int 4 = 1 then randomly_get_money g;
  g.money <- g.money +. earnings g;
  print_endline ("\n  Earnings for this round: $" ^ string_of_float (earnings g));
  age_tank g.tank;
  if g.round > 1 then growth_tank g.tank;
  health_tank g.tank ~-.5.;
  symbiosis g.tank;
  check_extinct g.tank;
  if g.round = g.max_rounds || g.money <= 0. then g.ended <- true
  else (
    health_reminder g;
    g.round <- g.round + 1)

(* PRINT FUNCTIONS *)

(** Summarizes the health of a player's fish. *)
let health_statement (g : game_state) =
  let playertank = g.tank in
  let body = ref "" in
  let header = ref "" in
  for i = 0 to num_species - 1 do
    body := !body ^ "           " ^ string_of_float playertank.(i).health;
    header := !header ^ "      " ^ string_of_species playertank.(i).species
  done;
  print_endline ("Health:       " ^ !body)
[@@coverage off]

let display_health (f : fish) : float = if extinct f then 0. else f.health

let print_fish (pstate : game_state) =
  let playertank = pstate.tank in
  let body1 = ref "" in
  let body2 = ref "" in
  let header = ref "" in
  for i = 0 to num_species - 1 do
    body1 := !body1 ^ "             " ^ string_of_int playertank.(i).num;
    body2 :=
      !body2 ^ "            "
      ^ (playertank.(i) |> display_health |> string_of_float);
    header := !header ^ "       " ^ string_of_species playertank.(i).species
  done;

  print_endline ("\n  Species: " ^ !header);
  print_endline ("  Count:" ^ !body1);
  print_endline ("  Health:" ^ !body2)
[@@coverage off]

let get_playermoney (g : game_state) : float = g.money
let get_max_rounds (g : game_state) : int = g.max_rounds
let get_round (g : game_state) : int = g.round

(** Initialize a round by printing the current round and currency. *)
let start_round_print (g : game_state) : string =
  "Day " ^ string_of_int g.round ^ "\nYou currently have $"
  ^ string_of_float g.money ^ "."
[@@coverage off]

(* FUNCTIONS TO AID TESTING *)

(** Given a fish, returns a sum of the species' ages*)
let get_age (f : fish) : int = f.age_sum

(** Given a fish, returns the current number*)
let get_num (f : fish) : int = f.num

(** Given a fgame and fish species, returns the current population*)
let get_species_num (g : game_state) (s : fish_species) : int =
  let i = fish_pos s in
  g.tank.(i).num

(** Given a game and a fish species, returns the health of the species*)
let get_health (g : game_state) (s : fish_species) : float =
  let i = fish_pos s in
  g.tank.(i).health

(** Given a game_state, returns the tank*)
let get_tank (g : game_state) = g.tank

type prey_record = {
  goldfish : int;
  anemone : int;
  clownfish : int;
  turtle : int;
}

(** Given a game_state, performs end of round shark update
    and returns record of number of each prey eaten *)
let get_eaten_prey (g : game_state) : prey_record =
  let prey_ar = shark_dinner g.tank in

  {
    goldfish = prey_ar.(0);
    anemone = prey_ar.(1);
    clownfish = prey_ar.(2);
    turtle = prey_ar.(3);
  }
