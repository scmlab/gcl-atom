type t =
  | WithState(State.t => Promise.t(list(t)))
  | SetSpecifications(array(Guacamole.GCL.Response.Specification.t))
  | AddDecorations(
      (array(Guacamole.GCL.Response.Specification.t), Atom.TextEditor.t) =>
      array(Atom.Decoration.t),
    )
  | DispatchCommand(Types.Command.t)
  | SendRequest(Types.Request.t)
  | Display(Guacamole.View.Request.header, Guacamole.View.Request.body);