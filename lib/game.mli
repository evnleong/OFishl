type fish_species =
  | Goldfish
  | Pufferfish
  | Shark
  | Huh  (** Type representing fish species *)

type fish_food

type fish
(** Type representing a population of fish. 
    Contains fields for the species name, how many, the collective age and health *)

type tank
type game_state

val add_fish : fish -> int -> unit
val price_fish : fish_species -> int -> float
val age_fish : fish -> unit
val age_tank : tank -> unit
val health_fish : fish -> float -> unit
val make_fish : fish_species -> fish_food -> fish
val health_tank_species : tank -> fish_species -> float -> unit
val health_tank : tank -> float -> unit
val med_game_species : game_state -> fish_species -> unit
val feed_fish_game : game_state -> fish_species -> int -> unit

val start_game : int -> game_state
(** Returns a new player state instance with n rounds. A player starts with $100 and 
    one nursery tank containing only the starting fish. *)

val set_game : game_state -> unit
(** Sets a player's tanks to an empty tank *)

val buy_fish_game : game_state -> fish_species -> int -> unit
val start_round_print : game_state -> string

(* val end_round_print : game_state -> string *)
val cost : game_state -> float -> unit
val earnings : game_state -> float
val health_reminder : game_state -> unit
val end_of_round : game_state -> unit
val health_statement : game_state -> unit
val print_fish : game_state -> unit
val get_max_rounds : game_state -> int
val buy_broke : game_state -> fish_species -> int -> bool
val med_broke : game_state -> bool
val species_alive : game_state -> fish_species -> bool
val string_of_fish_species : fish_species -> string
val feed_broke : game_state -> int -> bool
val predator_species : game_state -> fish_species -> bool

(* val print_fish_list : fish list -> string *)
(* val print_tank : tank -> string *)
val get_playermoney : game_state -> float
