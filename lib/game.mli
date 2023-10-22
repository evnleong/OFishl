type color
 
type fish
type tank
type tank_type
type player_state

val new_tank : tank_type -> tank
val add_fish : fish -> tank -> tank
val age_fish : fish -> int -> fish
val age_list : fish list -> fish list
val update_tank_ages : tank -> tank
val start_game : int -> player_state
val add_fish_game : player_state -> fish -> player_state
val start_round_print : player_state -> string
val end_round_print : player_state -> string
val cost : player_state -> int -> player_state
val end_of_round : player_state -> player_state
val fish_bio : fish -> string
val print_fish_list : fish list -> string
val print_tank : tank -> string
val make_fish : string -> string -> string -> fish
val goldfish : string -> fish
val pufferfish : string -> fish
val shark : string -> fish