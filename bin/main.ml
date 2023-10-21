(*opening Modules in lib*)
open Final_project

(*test terminal output*)
let () = print_endline "Hello, World!"

(*Prompt user for name input*)
let user_prompt () = ANSITerminal.print_string [ ANSITerminal.white ] "\n\n Give a Name for your fish> ";
read_line ()

(*create a red fish with custom name and output data to terminal*)
let y = Game.create_redfish (user_prompt ())
let _ = print_endline ("Your fish's name is " ^ (Game.get_name y) ^ " and it's color is " ^ (Game.get_color y))
let _ = print_endline "bye-bye"
