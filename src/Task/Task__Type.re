type t =
  | WithInstance(Types.Instance.t => Promise.t(list(t)))
  | SetSpecifications(array(GCL.Response.Specification.t))
  | AddDecorations(
      (array(GCL.Response.Specification.t), Atom.TextEditor.t) =>
      array(Atom.Decoration.t),
    )
  | DispatchRemote(Types.Command.remote)
  | DispatchLocal(Types.Command.local)
  | SendRequest(GCL.Request.t)
  | Display(Types.View.header, Body.t);
