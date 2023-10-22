(*opening Modules in lib*)
open Final_project

(*test terminal output*)
let () = print_endline "Build your own aquarium! Start the game with $100, use 
it to buy more fish and tanks. Give your aquatic friends cute names and watch them grow.
Each round simulates one day, the game ends after 3 rounds\n\nStart Game:\n"

(*Create a new game w/ 3 rounds*)
let g = Game.start_game 3


(*Round 1:*)
let _ = print_endline (Game.start_round_print g)
let _ = print_endline "Buy a goldfish for $2"
let user_prompt () = ANSITerminal.print_string [ ANSITerminal.white ] "Give a Name for your fish> ";
read_line ()
let g = Game.cost (Game.add_fish_game g (Game.goldfish (user_prompt ()))) 2
let _ = print_endline (Game.end_round_print g)
let g = Game.end_of_round g

(*Round 2:*)
let _ = print_endline (Game.start_round_print g)
let _ = print_endline "\nBuy a pufferfish for $5"
let user_prompt () = ANSITerminal.print_string [ ANSITerminal.white ] "Give a Name for your pufferfish> ";
read_line ()
let g = Game.cost (Game.add_fish_game g (Game.pufferfish (user_prompt ()))) 5
let _ = print_endline (Game.end_round_print g)
let g = Game.end_of_round g

(*Round 3:*)
let _ = print_endline (Game.start_round_print g)
let _ = print_endline "\nBuy a shark for $10"
let user_prompt () = ANSITerminal.print_string [ ANSITerminal.white ] "Give a Name for your shark> ";
read_line ()
let g = Game.cost (Game.add_fish_game g (Game.shark (user_prompt ()))) 10
let _ = print_endline (Game.end_round_print g)

let _ = print_endline "bye-bye"



