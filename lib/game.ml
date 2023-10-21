(*not currently used*)
type color = Red | Blue


type fish = {
  name : string;
  color : string;
}

let create_redfish (n:string) : fish = {name = n;color = "Red"}
let get_name (f:fish ): string = f.name
let get_color (f:fish) : string = f.color