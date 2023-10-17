module type MARINE_ANIMAL = sig 
   type species
   type size 
   type food 
   type stage
   type fish 

    val make_fish : string -> species -> size -> string 
    -> food -> int -> fish
end