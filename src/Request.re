type t =
  | Activate
  | Deactivate
  | Save;
let commandNames = [|"activate", "deactivate", "save"|];
let parse =
  fun
  | "activate" => Activate
  | "deactivate" => Deactivate
  | "save" => Save
  | _ => Save;
