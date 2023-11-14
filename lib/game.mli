type fish_species
type fish_food

type fish
(** Type representing a population of fish. 
    Contains fields for the species name, how many, the collective age and health *)

type tank
type player_state

type fish_species
(** Type representing fish species *)

type fish_food

(* val new_tank : fish list -> tank *)
val add_fish : fish -> int -> unit

(* val age_list : fish list -> fish list *)
val age_fish : fish -> unit
val age_tank : tank -> unit
val health_fish : fish -> int -> unit
val make_fish : fish_species -> fish_food -> fish
val health_tank : tank -> fish_species -> int -> unit

val start_game : int -> player_state
(** Returns a new player state instance with n rounds. A player starts with $100 and 
    one nursery tank containing only the starting fish. *)

val set_game : player_state -> unit
(** Sets a player's tanks to an empty tank *)

val add_fish_game : player_state -> fish_species -> int -> unit
val start_round_print : player_state -> string

(* val end_round_print : player_state -> string *)
val cost : player_state -> int -> player_state
val end_of_round : player_state -> unit
val print_fish : player_state -> unit

(* val print_fish_list : fish list -> string *)
(* val print_tank : tank -> string *)
val print_playermoney : player_state -> int
val goldfish : fish
val pufferfish : fish
val shark : fish
val goldfishspecies : fish_species
val pufferfishspecies : fish_species
val sharkspecies : fish_species
