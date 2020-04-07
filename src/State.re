open Belt;

module Error = Guacamole.State.Error;

type t = {
  editor: Atom.TextEditor.t,
  view: Types.View.t,
  mutable loaded: bool,
  mutable mode: Guacamole.View.Response.mode,
  mutable connection: option(Guacamole.Connection.t),
  mutable decorations: array(Atom.Decoration.t),
  mutable specifications: array(Guacamole.GCL.Response.Specification.t),
  mutable history: option(Types.Request.t),
};

let make = (editor: Atom.TextEditor.t): t => {
  let view = View.make(editor);
  let state = {
    editor,
    view,
    loaded: false,
    mode: WP1,
    connection: None,
    decorations: [||],
    specifications: [||],
    history: None,
  };

  // NOTE: dispose this!
  let _ = view.onSetMode.on(mode => {state.mode = mode});

  state;
};

let showView = state =>
  if (state.loaded) {
    state.view.setActivation(true) |> ignore;
  };

let hideView = state =>
  if (state.loaded) {
    state.view.setActivation(false) |> ignore;
  };

let displayError = (header, body, state) => {
  state.view.setHeader(Error(header)) |> ignore;
  state.view.setBody(Plain(body)) |> ignore;
};

// connect if not connected yet
let establishConnection =
    (state): Promise.t(result(Guacamole.Connection.t, Error.t)) => {
  switch (state.connection) {
  | None =>
    Guacamole.Connection.make(
      AtomImpl.Impl.Config.getGCLPath,
      AtomImpl.Impl.Config.setGCLPath,
    )
    ->Promise.mapError(e => Error.Connection(e))
    ->Promise.tapOk(conn => state.connection = Some(conn))
  | Some(connection) => Promise.resolved(Ok(connection))
  };
};

let sendRequest =
    (request, state): Promise.t(result(Guacamole.GCL.Response.t, Error.t)) => {
  let value = Types.Request.encode(request);
  Js.log2("<<<", value);

  let%Ok conn = state->establishConnection;
  let%Ok result =
    Guacamole.Connection.send(value, conn)
    ->Promise.mapError(e => Error.Connection(e));

  Js.log2(">>>", result);

  // catching exceptions occured when decoding JSON values
  switch (Guacamole.GCL.Response.decode(result)) {
  | value => Promise.resolved(Ok(value))
  | exception (Json.Decode.DecodeError(msg)) =>
    Promise.resolved(Error(Error.Decode(msg, result)))
  };
};

let cleanup = state => {
  hideView(state);
  state.loaded = false;

  state.decorations->Array.forEach(Atom.Decoration.destroy);
  state.specifications = [||];
  state.history = None;

  // connection
  state.connection
  ->Option.forEach(conn => {
      Guacamole.Connection.disconnect(conn)->ignore;
      state.connection = None;
    });
};

let destroy = state => {
  cleanup(state);
  module View = View.Impl(AtomImpl.Impl);
  View.destroy(state.view);
};