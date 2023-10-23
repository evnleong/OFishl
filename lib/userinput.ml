type input = Buy | View_Tanks | Wallet | Goldfish | Pufferfish | Shark

exception Invalid

let parse_input prompt =
  String.split_on_char ' ' prompt |> List.filter (fun s -> s <> "") |> function
  | x -> (
      match x with
      | [] -> raise Invalid
      | [ "Buy" ] -> Buy
      | [ "Tanks" ] -> View_Tanks
      | [ "Wallet" ] -> Wallet
      | [ "Goldfish" ] -> Goldfish
      | [ "Pufferfish" ] -> Pufferfish
      | [ "Shark" ] -> Shark
      | [ _ ] -> raise Invalid
      | _ -> raise Invalid)
