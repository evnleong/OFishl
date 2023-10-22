type color
type species 
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
val game_status : player_state -> string
val make_fish : string -> species -> string -> fish
val goldfish : string -> fish
val fish_bio : fish -> string
val get_name : fish -> string
val get_color : fish -> string 
val get_round : player_state -> int