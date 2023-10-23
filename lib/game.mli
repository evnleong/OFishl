type fish
type tank
type player_state

val new_tank : string -> tank
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
val print_playermoney : player_state -> int
val make_fish : string -> string -> string -> fish
val goldfish : string -> fish
val pufferfish : string -> fish
val shark : string -> fish
val hunger_fish : fish -> int -> fish
val hunger_list : fish list -> fish list
val feed_fish : fish -> int -> fish
val feed_list : fish list -> fish list
val feed_tank : tank -> tank
val feed_game : player_state -> player_state
