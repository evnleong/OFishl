type input = Buy | Feed | Medicine | View_Tanks | Wallet | Instructions | Pass | Dunno

val parse : string -> string list 

val parse_input : string -> input

val parse_species : string -> Game.fish_species

val parse_species_int : string -> Game.fish_species * int
