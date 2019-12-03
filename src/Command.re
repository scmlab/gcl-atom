type t =
  | Toggle
  | Save
  | Refine;

let commandNames = [|"toggle", "save", "refine"|];
let parse =
  fun
  | "toggle" => Toggle
  | "save" => Save
  | "refine" => Refine
  | _ => Save;
