type t =
  | WithInstance(Instance.t => Promise.t(list(t)))
  | SetSpecifications(array(Response.Specification.t))
  | AddDecorations(
      (array(Response.Specification.t), Atom.TextEditor.t) =>
      array(Atom.Decoration.t),
    )
  | DispatchCommand(Types.Command.t)
  | SendRequest(Types.Request.t)
  | Display(Types.View.header, Body.t);
