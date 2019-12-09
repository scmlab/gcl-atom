module Raw = {
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
};

// elaborated command
type t =
  | Raw(Raw.t)
  | Activate
  | Deactivate
  | Update(string)
  | Refine(Response.Specification.t);
