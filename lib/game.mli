type fish_species
type fish_food
type fish

val species_to_string : fish -> string
val num_species : int

type tank
type player_state

val make_fish : fish_species -> fish_food -> fish
val set_tank : tank -> unit
val fish_pos : fish_species -> int
val age_fish : fish -> unit
val age_tank : tank -> unit
val health_fish : fish -> int -> unit
val health_tank : tank -> fish_species -> int -> unit
val start_game : int -> player_state
val set_game : player_state -> unit
val add_fish : fish -> int -> unit
val add_fish_tank : tank -> fish_species -> int -> unit
val add_fish_game : player_state -> fish_species -> int -> unit
val end_of_round : player_state -> unit
val print_fish : fish -> string
val print_playermoney : player_state -> int
val start_round_print : player_state -> string
val end_round_print : player_state -> string
val cost : player_state -> int -> unit
