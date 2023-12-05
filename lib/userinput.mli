type input =
  | Buy
  | Feed
  | Medicine
  | View_Tanks
  | Wallet
  | Manual
  | Pass
  | Dunno  (** Input types representing possible user inputs *)

val parse : string -> string list
(** Helper function for parsing user input strings to string lists *)

val parse_input : string -> input
(** Helper function for converting parsed strings from player to values of type input. *)

val parse_species : string -> Game.fish_species
(** Helper function for parsing user input to fish species *)

val parse_species_int : string -> Game.fish_species * int
(** Helper function for parsing user input to fish species and their count *)
