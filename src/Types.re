module View = {
  type mode =
    | WP1
    | WP2;

  type header =
    | Loading
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
      onSetMode: Event.t(mode),
      link: Event.t(Link.event),
    };

    let make = () => {
      updateConnection: Channel.make(),
      setActivation: Channel.make(),
      setHeader: Channel.make(),
      setBody: Channel.make(),
      onSetMode: Event.make(),
      link: Event.make(),
    };
  };
  module Events = {
    type t = {onSetMode: Event.t(mode)};

    let make = () => {
      {onSetMode: Event.make()};
    };
  };

  // Out facing interface of the view
  module Interface = {
    type t = {
      setActivation: bool => Promise.t(unit),
      setHeader: header => Promise.t(unit),
      setBody: Body.t => Promise.t(unit),
      onSetMode: Event.t(mode),
    };

    let make = (channels: Channels.t, events: Events.t) => {
      setActivation: Channel.sendTo(channels.setActivation),
      setHeader: Channel.sendTo(channels.setHeader),
      setBody: Channel.sendTo(channels.setBody),
      onSetMode: events.onSetMode,
    };
  };
};

module Request = {
  type t =
    | Load(string)
    | Load2(string)
    | Refine(int, string)
    | InsertAssertion(int)
    | Debug;

  module Encode = {
    open Json.Encode;
    let request: encoder(t) =
      fun
      | Load(filepath) =>
        object_([("tag", string("Load")), ("contents", string(filepath))])
      | Load2(filepath) =>
        object_([
          ("tag", string("Load2")),
          ("contents", string(filepath)),
        ])
      | Refine(id, payload) =>
        object_([
          ("tag", string("Refine")),
          ("contents", (id, payload) |> pair(int, string)),
        ])
      | InsertAssertion(n) =>
        object_([
          ("tag", string("InsertAssertion")),
          ("contents", int(n)),
        ])
      | Debug => object_([("tag", string("Debug"))]);
  };

  let encode: t => string = x => x->Encode.request->Json.stringify;
};

module Command = {
  type t =
    | Toggle
    | Save
    | Refine
    | InsertAssertion
    | Debug;

  let names = [|"toggle", "save", "refine", "insert-assertion", "debug"|];

  let parse =
    fun
    | "toggle" => Toggle
    | "save" => Save
    | "refine" => Refine
    | "insert-assertion" => InsertAssertion
    | "debug" => Debug
    | _ => Save;
};