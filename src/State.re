open Belt;

module Error = {
  type t =
    | Connection(Connection.Error.t)
    | Decode(string, Js.Json.t);

  let toString =
    fun
    | Connection(e) => Connection.Error.toString(e)
    | Decode(msg, json) => (
        {js|JSON Decode Error|js},
        msg ++ "\n" ++ "JSON from GCL: \n" ++ Js.Json.stringify(json),
      );
};

type t = {
  editor: Atom.TextEditor.t,
  view: Types.View.Interface.t,
  mutable loaded: bool,
  mutable connection: option(Connection.t),
  mutable decorations: array(Atom.Decoration.t),
  mutable specifications: array(Response.Specification.t),
  mutable history: option(Types.Request.t),
};

let make = (editor: Atom.TextEditor.t): t => {
  editor,
  view: View.make(editor),
  loaded: false,
  connection: None,
  decorations: [||],
  specifications: [||],
  history: None,
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
let establishConnection = (state): Promise.t(result(Connection.t, Error.t)) => {
  switch (state.connection) {
  | None =>
    Connection.make()
    ->Promise.mapError(e => Error.Connection(e))
    ->Promise.tapOk(conn => state.connection = Some(conn))
  | Some(connection) => Promise.resolved(Ok(connection))
  };
};

let sendRequest = (request, state): Promise.t(result(Response.t, Error.t)) => {
  let value = Types.Request.encode(request);
  Js.log2("<<<", value);

  let%Ok conn = state->establishConnection;
  let%Ok result =
    Connection.send(value, conn)->Promise.mapError(e => Error.Connection(e));

  Js.log2(">>>", result);

  // catching exceptions occured when decoding JSON values
  switch (Response.decode(result)) {
  | value => Promise.resolved(Ok(value))
  | exception (Json.Decode.DecodeError(msg)) =>
    Promise.resolved(Error(Error.Decode(msg, result)))
  };
};

let destroy = state => {
  state.loaded = false;
  state.view.setActivation(false)->ignore;
  state.editor->View.destroy;

  state.decorations->Array.forEach(Atom.Decoration.destroy);
  state.specifications = [||];
  state.history = None;

  // connection
  state.connection
  ->Option.forEach(conn => {
      Connection.disconnect(conn)->ignore;
      state.connection = None;
    });
};