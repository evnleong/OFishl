type input = Buy | View_Tanks | Wallet | Feed | Dunno | Instructions

(** Converts string to value of type input. *)
let parse_input prompt : input = 
  prompt |> String.uppercase_ascii |> String.split_on_char ' ' 
    |> List.filter (fun s -> s <> "" && s <> " ") |> function | x -> (
    match x with 
    | [ "BUY" ] -> Buy
    | [ "FEED" ] -> Feed
    | [ "TANKS" ] -> View_Tanks
    | [ "WALLET" ] -> Wallet
    | [ "INSTRUCTIONS" ] -> Instructions
    | _ -> Dunno )

let parse_species (prompt : string) : Game.fish_species = 
  match prompt with 
  | "GOLDFISH" -> Goldfish
  | "PUFFERFISH" -> Pufferfish
  | "SHARK" -> Shark
  | _ -> Huh 

let parse_species_int (prompt : string) : Game.fish_species * int = 
  let lst = prompt |> String.uppercase_ascii |> String.split_on_char ' ' 
    |> List.filter (fun s -> s <> "" && s <> " ") in
  if List.length lst <> 2 then (Huh, 0)
  else match Stdlib.int_of_string_opt (List.nth lst 1) with 
    | None -> (Huh, 0)
    | Some n -> begin match parse_species (List.nth lst 0) with 
      | Huh -> (Huh, 0)
      | x -> (x, n)
    end