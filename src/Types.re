module View = {
  // Internal channels for facilitating communication with view components
  module Channels = {
    type t = {
      updateConnection:
        Channel.t(
          (Guacamole.Connection.t, option(AgdaMode.Process.Error.t)),
          unit,
        ),
      setActivation: Channel.t(bool, unit),
      setHeader: Channel.t(Guacamole.View.Request.Header.t, unit),
      setBody: Channel.t(Guacamole.View.Request.Body.t, unit),
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
      onSetMode: Event.t(Guacamole.View.Response.mode),
      onLink: Event.t(Guacamole.View.Response.linkEvent),
    };

    let make = () => {onSetMode: Event.make(), onLink: Event.make()};
  };

  // Out facing interface of the view
  type t = {
    editor: Atom.TextEditor.t,
    element: Webapi.Dom.Element.t,
    subscriptions: array(unit => unit),
    onRequest: Guacamole.Event.t(Guacamole.View.Request.t),
    onResponse: Guacamole.Event.t(Guacamole.View.Response.t),
    // setActivation: bool => Promise.t(unit),
    // setHeader: Guacamole.View.Request.Header.t => Promise.t(unit),
    // setBody: Guacamole.View.Request.Body.t => Promise.t(unit),
    // onSetMode: Event.t(Guacamole.View.Response.mode),
    // onLink: Event.t(Guacamole.View.Response.linkEvent),
  };

  let make =
      (
        editor: Atom.TextEditor.t,
        element: Webapi.Dom.Element.t,
        onRequest: Guacamole.Event.t(Guacamole.View.Request.t),
        onResponse: Guacamole.Event.t(Guacamole.View.Response.t),
      ) => {
    editor,
    element,
    subscriptions: [||],
    onRequest,
    onResponse,
    // setActivation: Channel.sendTo(channels.setActivation),
    // setHeader: Channel.sendTo(channels.setHeader),
    // setBody: Channel.sendTo(channels.setBody),
    // onSetMode: events.onSetMode,
    // onLink: events.onLink,
  };

  let destroy = self => {
    self.subscriptions->Belt.Array.forEach(destructor => destructor());
  };

  let send = (self, request) => {
    self.onRequest.emit(request);
    Promise.resolved(true);
  };

  let recv = (self, callback) => {
    self.onResponse.on(callback)->Js.Array.push(self.subscriptions)->ignore;
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