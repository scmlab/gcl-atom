module View = {
  type header =
    | AllGood
    | Plain(string)
    | Error(string);

  // Internal channels for facilitating communication with view components
  module Channels = {
    type t = {
      updateConnection:
        Channel.t((Connection.t, option(AgdaMode.Process.Error.t)), unit),
      setActivation: Channel.t(bool, unit),
      setHeader: Channel.t(header, unit),
      setBody: Channel.t(Body.t, unit),
      link: Event.t(Link.event),
    };

    let make = () => {
      updateConnection: Channel.make(),
      setActivation: Channel.make(),
      setHeader: Channel.make(),
      setBody: Channel.make(),
      link: Event.make(),
    };
  };

  // Out facing interface of the view
  module Interface = {
    type t = {
      setActivation: bool => Promise.t(unit),
      setHeader: header => Promise.t(unit),
      setBody: Body.t => Promise.t(unit),
    };

    let make = (channels: Channels.t) => {
      setActivation: Channel.sendTo(channels.setActivation),
      setHeader: Channel.sendTo(channels.setHeader),
      setBody: Channel.sendTo(channels.setBody),
    };
  };
};

module Command = {
  type remote =
    | Load(string)
    | Refine(Specification.t)
    | InsertAssertion(int)
    | Debug;
  type local =
    | Toggle
    | Save
    | Refine
    | InsertAssertion
    | Debug;
};

module Instance = {
  type t = {
    editor: Atom.TextEditor.t,
    view: View.Interface.t,
    mutable toggle: bool,
    mutable connection: option(Connection.t),
    mutable decorations: array(Atom.Decoration.t),
    mutable specifications: array(Specification.t),
    mutable history: option(Command.remote),
  };
};

module Task = {
  type t =
    | WithInstance(Instance.t => Promise.t(list(t)))
    | SetSpecifications(array(Specification.t))
    | AddDecorations(
        (array(Specification.t), Atom.TextEditor.t) =>
        array(Atom.Decoration.t),
      )
    | DispatchRemote(Command.remote)
    | DispatchLocal(Command.local)
    | SendRequest(Request.t)
    | Display(View.header, Body.t);
};
