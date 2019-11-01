module View = {
  type header =
    | AllGood
    | Plain(string)
    | Error(string);

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
type instance = {
  editor: Atom.TextEditor.t,
  view: View.Interface.t,
  mutable connection: Connection.t,
  mutable decorations: array(Atom.Decoration.t),
};
