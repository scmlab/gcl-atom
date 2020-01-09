open! Rebase;

module Local = {
  open Types.Command;
  type t = Types.Command.local;
  let commandNames = [|"toggle", "save", "refine", "debug"|];
  let parse =
    fun
    | "toggle" => Toggle
    | "save" => Save
    | "refine" => Refine
    | "debug" => Debug
    | _ => Save;
};

module Remote = {
  open Types.Command;
  open Types.Task;
  type t = Types.Command.remote;
};
