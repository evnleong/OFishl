(*opening Modules in lib*)
open Final_project

(*test terminal output*)
let () = print_endline "Build your own aquarium!"

(*Create a new game w/ 10 rounds*)
let g = Game.start_game 10
let _ = print_endline (Game.game_status g)

(*Prompt user for name input*)
let user_prompt () = ANSITerminal.print_string [ ANSITerminal.white ] "\n\n Give a Name for your fish> ";
read_line ()

(*create a red fish with custom name and output data to terminal*)
let y = Game.goldfish (user_prompt ())
let _ = print_endline (Game.fish_bio y)
let _ = print_endline "bye-bye"

