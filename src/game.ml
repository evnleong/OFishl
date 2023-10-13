module MarineAnimal = struct
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
    hunger : bool;
    age : int;
    stage : stage;
  }

  (**Creates a baby goldfish*)
  let goldfish (name : string) : fish =
    {
      name;
      species = Goldfish;
      size = Small;
      color = "orange";
      food = Flakes;
      hp = 100;
      hunger = true;
      age = 0;
      stage = Baby;
    }
  
  (**Creates a baby pufferfish*)
  let pufferfish (name : string) : fish =
    {
      name;
      species = Pufferfish;
      size = Medium;
      color = "yellow";
      food = Algae;
      hp = 100;
      hunger = true;
      age = 0;
      stage = Baby;
    }
  (**Creates a baby shark*)
  let shark (name : string) : fish =
    {
      name;
      species = Shark;
      size = Large;
      color = "grey";
      food = Fish;
      hp = 100;
      hunger = true;
      age = 0;
      stage = Baby;
    }
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
   let add_fish (f:MarineAnimal.fish) (t:tank) : tank = {
    tank_type = t.tank_type; 
    fish_list = f::t.fish_list; 
    capacity = t.capacity - 1;
   }
end