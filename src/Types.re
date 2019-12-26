module View = {
  type header =
    | AllGood
    | Plain(string)
    | Error(string);

  // Internal channels for facilitating communication with view components
  module Channels = {
    type t = {
      updateConnection:
        Channel.t((Connection.t, option(Connection.Error.t)), unit, unit),
      setActivation: Channel.t(bool, unit, unit),
      setHeader: Channel.t(header, unit, unit),
      setBody: Channel.t(Body.t, unit, unit),
    };

    let make = () => {
      updateConnection: Channel.make(),
      setActivation: Channel.make(),
      setHeader: Channel.make(),
      setBody: Channel.make(),
    };
  };

  // Out facing interface of the view
  module Interface = {
    type t = {
      setActivation: bool => Async.t(unit, unit),
      setHeader: header => Async.t(unit, unit),
      setBody: Body.t => Async.t(unit, unit),
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
    | Refine(Response.Specification.t)
  and local =
    | Toggle
    | Save
    | Refine
  and task('a) =
    | WithInstance('a => Async.t(list(task('a)), unit))
    | DispatchRemote(remote)
    | DispatchLocal(local)
    | SendRequest(Request.t)
    | Display(View.header, Body.t);
};

module Instance = {
  type t = {
    editor: Atom.TextEditor.t,
    view: View.Interface.t,
    mutable toggle: bool,
    mutable connection: Connection.t,
    mutable decorations: array(Atom.Decoration.t),
    mutable specifications: array(Response.Specification.t),
    mutable history: option(Command.local),
  };
};
