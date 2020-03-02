open! Rebase;

module Local = {
  open Types.Command;
  type t = Types.Command.local;
  let commandNames = [|
    "toggle",
    "save",
    "refine",
    "insert-assertion",
    "debug",
  |];
  let parse =
    fun
    | "toggle" => Toggle
    | "save" => Save
    | "refine" => Refine
    | "insert-assertion" => InsertAssertion
    | "debug" => Debug
    | _ => Save;
};
