type input = Buy | View_Tanks | Wallet | Feed | Goldfish | Pufferfish | Shark

exception Invalid

let parse_input prompt =
  String.split_on_char ' ' prompt |> List.filter (fun s -> s <> "") |> function
  | x -> (
      match x with
      | [] -> raise Invalid
      | [ "BUY" ] -> Buy
      | [ "TANKS" ] -> View_Tanks
      | [ "WALLET" ] -> Wallet
      | [ "GOLDFISH" ] -> Goldfish
      | [ "PUFFERFISH" ] -> Pufferfish
      | [ "SHARK" ] -> Shark
      | [ "FEED" ] -> Feed
      | [ _ ] -> raise Invalid
      | _ -> raise Invalid)
