type fish_species =
  | Goldfish
  | Anemone
  | Clownfish
  | Turtle
  | Remora
  | Shark
  | Huh  (** Type representing the game's fish species *)

(*  | Lancetfish *)

type fish_food =
  | Fish
  | Pellet  (** Type representing the type of fish food available*)

type fish
(** Type representing a population of fish. 
    Contains fields for the species name, species count, and the species collective age and health *)

type tank
(** An array that stores all of a player's fish populations*)

type game_state
(** Type representing the current state of the player's game*)

val make_fish : fish_species -> fish_food -> float -> float -> fish
(** Given a species, food, growth_rate, and death_rate, creates a new fish with age = 0*)

val add_fish : fish -> int -> unit
(** Takes in a fish population, f, and an int, n, and adds n fish to f *)

val price_fish : fish_species -> int -> float
(** Takes in a fish species, s, and an int, n, and returns the value of buying n of fish type s. *)

val age_fish : fish -> unit
(** Ages a given fish population f by one round. In effect, f's age sum value will increase
    by the total number of fish in f. *)

val age_tank : tank -> unit
(** Ages every fish population in a tank by one round. *)

val health_fish : fish -> float -> unit
(** Takes float i and fish population f and adds i to the health of f. *)

val health_tank_species : tank -> fish_species -> float -> unit
(** Takes a tank t, a fish species s, and a given float i. Adds i to the health of fish population of species s in tank t. *)

val health_tank : tank -> float -> unit
(** Takes a tank t, and float i. Adds i to the health of every fish population in tank t. *)

val med_game_species : game_state -> fish_species -> unit
(** Takes current game state g, and fish species s, and feeds medicine to fish population of species s in g. *)

val feed_fish_game : game_state -> fish_species -> int -> unit
(** Takes current game state g, fish species s, and an int n, and feeds n pellets to s*)

val start_game : int -> game_state
(** Takes an int, n, and returns a new player state instance with n rounds. A player starts with $100 and 
    one nursery tank containing only the starting fish. *)

val set_game : game_state -> unit
(** Takes current game state g, and sets all of the player's tanks to an empty tank *)

val buy_fish_game : game_state -> fish_species -> int -> unit
(** Adds n fish to fish population of species s in game state g. Subtracts
    cost of the fish from g's money. *)

val start_round_print : game_state -> string
(** Initialize a given game state, g,  by printing the current round and currency of g. *)

val cost : game_state -> float -> unit
(** Takes float i and a game state g, and subtracts i cost from game state g's money. *)

val earnings : game_state -> float
(** Takes a game state g and calculates the total value of a player's fish populations in g and returns this value as a float. *)

val health_reminder : game_state -> unit
(** Takes in a current game state g, and reminds the player if a species needs to be fed. *)

val end_of_round : game_state -> unit
(** Takes in a current game state g, and ages all fish population ages by one round. *)

val health_statement : game_state -> unit
(** Takes a current game state g and returns a health summary of all fish populations in g. *)

val print_fish : game_state -> unit
(** Takes a current game state g and prints out tank information for g.*)

val get_max_rounds : game_state -> int
(** Takes a current game state g and retrieves the max # of rounds set for g.*)

val buy_broke : game_state -> fish_species -> int -> bool
(** Takes a game state g and fish species s and returns whether player can buy fish of species s. *)

val med_broke : game_state -> bool
(* Takes a game state g and fish species s and returns whether player has enough money to buy medicine *)

val species_extinct : game_state -> fish_species -> bool
(** Takes a game state g and returns whether species s is extinct in game g. *)

val string_of_fish_species : fish_species -> string
(** Takes a fish species s and returns a string representing that fish species*)

val plural_species : fish_species -> string
(** Takes a fish species s and returns a plural string representing that fish species*)

val feed_broke : game_state -> int -> bool
(** Takes a game state g and an int n and returns whether player has enough money to feed n food *)

val predator_species : game_state -> fish_species -> bool
(** Helper function to check if species s in game g is a predator. *)

val plural_species : fish_species -> string
(** Helper formatting function to return the correct plural name for a given fish species s *)

val get_playermoney : game_state -> float
(** Takes a current game state g and retrieves the player's current money total *)

val growth_tank : tank -> unit
(** Updates count of each fish population in a tank. *)

(* FUNCTIONS FOR TESTING*)

val get_age : fish -> int
(** Given a fish, returns the age_sum*)

val get_num : fish -> int
(** Given a fish, returns the population*)

val get_health : game_state -> fish_species -> float
(** Given a fish, returns the health*)

val get_tank : game_state -> tank
(** Given a game_state, returns the tank*)

val get_species_population : game_state -> fish_species -> int
(** Given a game_state and fish species, returns the current population*)

(* Deprecated Functions *)

(* val end_round_print : game_state -> string *)
(* val print_fish_list : fish list -> string *)
(* val print_tank : tank -> string *)
