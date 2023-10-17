module type MARINE_ANIMAL = sig 
  type species
  type size 
  type food 
  type stage
  type fish 
  val make_fish : string -> species -> size -> string 
    -> food -> int -> fish
end

module MarineAnimal : MARINE_ANIMAL = struct
  type species = Goldfish | Pufferfish | Shark
  type size = Small | Medium | Large
  type food = Flakes | Algae | Fish
  type stage = Baby | Adult

  type fish = {
    name : string;
    species : species;
    size : size;
    color : string;
    food : food;
    hp : int;
    hunger : int;
    age : int;
    stage : stage;
  }

  (* Remainder from subtraction a - b. *)
  let rem (a : int) (b : int) : int * int =
    if b <= a then (a-b, 0) else (0, b-a)

  (* Given fish, food type, and amount of food, returns updated fish and amount
    of food. For example, if the food type is not appropriate for the fish, or 
    if the fish isn't hungry, then the amount of food stays the same. *)
  let feed (f : fish) (fd : food) (amt : int): fish * int =
    if fd = f.food then
      ({f with hunger = fst (rem f.hunger amt)},
      snd (rem f.hunger amt))
    else (f, amt)

  (** Makes a new fish. *)
  let make_fish (n : string) (sp : species) (sz : size) (color : string) 
  (fd : food) (hunger : int) : fish = { 
    name = n;
    species = sp; 
    size = sz; 
    color = color; 
    food = fd; 
    hp = 100; 
    hunger = hunger; 
    age = 0;
    stage = Baby 
  }

  (** Creates a baby goldfish. *)
  let goldfish (name : string) : fish =
    make_fish name Goldfish Small "orange" Flakes 10
  
  (** Creates a baby pufferfish. *)
  let pufferfish (name : string) : fish =
    make_fish name Pufferfish Medium "yellow" Algae 20
  
  (** Creates a baby shark. *)
  let shark (name : string) : fish =
    make_fish name Shark Large "grey" Fish 50
end

module Tanks = struct
  type tank_type = Nursery | AdultTank

  type tank = { 
    tank_type : tank_type; 
    fish_list : MarineAnimal.fish list; 
    capacity : int 
    }

  (** Given a tank type, creates an empty tank*)
  let new_tank (t : tank_type) : tank = { 
    tank_type = t; 
    fish_list = []; 
    capacity = 10; 
    }
    
   (** Given a tank and a fish, adds the fish to the tank*)
   let add_fish (f : MarineAnimal.fish) (t : tank) : tank = {
    tank_type = t.tank_type; 
    fish_list = f::t.fish_list; 
    capacity = t.capacity - 1;
   }
end

let max_rounds = 5

type player_state = {
  money : int;
  round : int;
  tanks : Tanks.tank list 
}