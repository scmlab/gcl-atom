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
  | Activate
  | Deactivate
  | Update(string)
  | Refine(Response.Specification.t);

type task =
  | Others(Type.Instance.t => Async.t(list(task), unit))
  | Dispatch(t)
  | DispatchRaw(Raw.t)
  | SendRequest(Request.t)
  | Display(Type.View.header, Body.t);
