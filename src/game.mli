module type MARINE_ANIMAL = sig
  type species
  type size
  type food
  type stage
  type fish

  val make_fish : string -> species -> size -> string -> food -> int -> fish
  (** Makes a new fish. *)

  val goldfish : string -> fish
  (** Creates a baby goldfish. *)

  val pufferfish : string -> fish
  (** Creates a baby pufferfish. *)

  val shark : string -> fish
  (** Creates a baby shark. *)

  val age_fish : fish -> int -> fish
  (**Given a fish and an integer i, adds i to the fish's age*)

  val age_list : fish list -> fish list
  (**Given a list of fish, updates each the age of each fish by one round*)
end
