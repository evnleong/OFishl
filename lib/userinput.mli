type input = Buy | View_Tanks | Wallet | Feed | Dunno | Instructions

val parse_input : string -> input

val parse_species_int : string -> Game.fish_species * int
