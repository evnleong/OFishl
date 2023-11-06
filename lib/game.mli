type fish
type tank
type player_state

val new_tank : fish list -> tank
val add_fish : fish -> tank -> tank
val age_fish : fish -> fish
val age_list : fish list -> fish list
val age_tank : tank -> tank
val health_fish : fish -> int -> fish
val health_list : fish list -> int -> fish list
val health_tank : tank -> int -> tank
val start_game : int -> player_state
val add_fish_game : player_state -> fish -> player_state
val start_round_print : player_state -> string
val end_round_print : player_state -> string
val cost : player_state -> int -> player_state
val end_of_round : player_state -> player_state
val print_fish : fish -> string
val print_fish_list : fish list -> string
val print_tank : tank -> string
val print_playermoney : player_state -> int
