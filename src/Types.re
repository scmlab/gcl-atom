module View = {
  type mode =
    | WP1
    | WP2;

  type header =
    | Loading
    | Plain(string)
    | Error(string);

  //
  type request =
    | Show
    | Hide
    | Display(header, Body.t);
  type response =
    | SetMode(mode)
    | Link(Link.event);

  // Internal channels for facilitating communication with view components
  module Channels = {
    type t = {
      updateConnection:
        Channel.t(
          (Guacamole.Connection.t, option(AgdaMode.Process.Error.t)),
          unit,
        ),
      setActivation: Channel.t(bool, unit),
      setHeader: Channel.t(header, unit),
      setBody: Channel.t(Body.t, unit),
    };

    let make = () => {
      updateConnection: Channel.make(),
      setActivation: Channel.make(),
      setHeader: Channel.make(),
      setBody: Channel.make(),
    };
  };

  module Events = {
    type t = {
      onSetMode: Event.t(mode),
      onLink: Event.t(Link.event),
    };

    let make = () => {onSetMode: Event.make(), onLink: Event.make()};
  };

  // Out facing interface of the view
  type t = {
    editor: Atom.TextEditor.t,
    element: Webapi.Dom.Element.t,
    subscriptions: array(unit => unit),
    setActivation: bool => Promise.t(unit),
    setHeader: header => Promise.t(unit),
    setBody: Body.t => Promise.t(unit),
    onSetMode: Event.t(mode),
    onLink: Event.t(Link.event),
  };

  let make =
      (
        editor: Atom.TextEditor.t,
        element: Webapi.Dom.Element.t,
        channels: Channels.t,
        events: Events.t,
      ) => {
    editor,
    element,
    subscriptions: [||],
    setActivation: Channel.sendTo(channels.setActivation),
    setHeader: Channel.sendTo(channels.setHeader),
    setBody: Channel.sendTo(channels.setBody),
    onSetMode: events.onSetMode,
    onLink: events.onLink,
  };

  let destroy = self => {
    self.subscriptions->Belt.Array.forEach(destructor => destructor());
  };

  let send = self =>
    fun
    | Show => self.setActivation(true) |> ignore
    | Hide => self.setActivation(false) |> ignore
    | Display(header, body) => {
        self.setHeader(header) |> ignore;
        self.setBody(body) |> ignore;
      };

  let recv = (self, callback) => {
    self.onSetMode.on(x => callback(SetMode(x)))
    ->Js.Array.push(self.subscriptions)
    ->ignore;
    self.onLink.on(x => callback(Link(x)))
    ->Js.Array.push(self.subscriptions)
    ->ignore;
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

  let names = [|"toggle", "load", "refine", "insert-assertion", "debug"|];

  let parse =
    fun
    | "toggle" => Toggle
    | "load" => Save
    | "refine" => Refine
    | "insert-assertion" => InsertAssertion
    | "debug" => Debug
    | _ => Save;
};