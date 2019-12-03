type t =
  | Toggle
  | Save;
let commandNames = [|"toggle", "save"|];
let parse =
  fun
  | "toggle" => Toggle
  | "save" => Save
  | _ => Save;
