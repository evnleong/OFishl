
(*not currently used*)
type color = Red | Blue | Orange | Grey | Yellow
type species = Goldfish | Pufferfish | Shark

type fish = {
  name : string;
  species : species;
  color : string;
  age : int
}

type tank_type = Nursery | AdultTank

type tank = { 
    tank_type : tank_type; 
    fish_list : fish list; 
    capacity : int 
    }

let new_tank (t : tank_type) : tank = { 
  tank_type = t; 
  fish_list = []; 
  capacity = 10 }

let add_fish (f : fish) (t : tank) : tank = {
  tank_type = t.tank_type;
  fish_list = f :: t.fish_list;
  capacity = t.capacity - 1;
}

(**Given a fish and an integer i, adds i to the fish's age*)
let age_fish (f : fish) (i : int) : fish = {
    name = f.name;
    color = f.color;
    species = f.species;
    age = f.age + i;
  }

(**Given a list of fish, updates each the age of each fish by one round*)
let rec age_list (lst : fish list) : fish list =
  match lst with [] -> lst | h :: t -> age_fish h 1 :: age_list t

 (**Given a tank, updates the age of every fish in the tank by one round*)
 let update_tank_ages (t : tank) : tank =
  {
    tank_type = t.tank_type;
    fish_list = age_list t.fish_list;
    capacity = t.capacity;
  }

type player_state = {
  round : int;
  money : int;
  tanks : tank list;
  max_rounds : int;
}

let start_game (i:int) : player_state = {
  round = 1;
  money = 100;
  tanks = [new_tank Nursery];
  max_rounds = i
}

let get_money (g:player_state): int = g.money

let game_status (g:player_state) : string = 
  ("Day " ^(string_of_int g.round)^": \nYou have $" ^ (string_of_int (g.money)) ^ ".")

let fish_bio (f:fish) : string = "Your fish's name is " ^ (f.name) ^ " and it's color is " ^ (f.color) 

let make_fish (n:string) (sp:species) (color:string): fish = {
  name =n;
  species =sp;
  color = color;
  age = 0;
}

let goldfish (n:string) : fish = make_fish n Goldfish "orange"

let get_name (f:fish ): string = f.name

let get_color (f:fish) : string = f.color

let get_round (g:player_state): int = g.round