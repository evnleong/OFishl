type input = Buy | Feed | Medicine | View_Tanks | Wallet | Instructions | Pass | Dunno

let parse (prompt : string) : string list = 
  prompt |> String.uppercase_ascii |> String.split_on_char ' ' 
    |> List.filter (fun s -> s <> "" && s <> " ")

(** Converts string to value of type input. *)
let parse_input (prompt : string) : input = 
  prompt |> parse |> function | x -> (
    match x with 
    | [ "BUY" ] -> Buy
    | [ "FEED" ] -> Feed
    | [ "MEDICINE" ] -> Medicine
    | [ "TANKS" ] -> View_Tanks
    | [ "WALLET" ] -> Wallet
    | [ "INSTRUCTIONS" ] -> Instructions
    | [ "PASS" ] -> Pass
    | _ -> Dunno )

(** Converts string to value of type fish_species. *)
let parse_species (prompt : string) : Game.fish_species = 
  match parse prompt with 
  | [ "GOLDFISH" ] -> Goldfish 
  | [ "PUFFERFISH" ] -> Pufferfish 
  | [ "SHARK" ] -> Shark 
  | _ -> Huh

(** Converts parsed string to value of type fish_species. *)
let parse_species_str (prompt : string) : Game.fish_species = 
  match prompt with 
  | "GOLDFISH" -> Goldfish
  | "PUFFERFISH" -> Pufferfish
  | "SHARK" -> Shark
  | _ -> Huh 

let parse_species_int (prompt : string) : Game.fish_species * int = 
  let lst = parse prompt in
  if List.length lst <> 2 then (Huh, 0)
  else match Stdlib.int_of_string_opt (List.nth lst 1) with 
    | None -> (Huh, 0)
    | Some n -> begin match parse_species_str (List.nth lst 0) with 
      | Huh -> (Huh, 0)
      | x -> (x, n)
    end