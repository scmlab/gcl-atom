open! Rebase;
open Rebase.Fn;

open! Types.Instance;

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

let make = (editor: Atom.TextEditor.t): t => {
  editor,
  view: View.make(editor),
  toggle: false,
  connection: None,
  decorations: [||],
  specifications: [||],
  history: None,
};

module View = {
  // destroy the view
  let destroy = instance => instance.editor |> View.destroy;
  let show = instance => instance.view.setActivation(true) |> ignore;
  let hide = instance => instance.view.setActivation(false) |> ignore;
  let displayError = (header, body, instance) => {
    instance.view.setHeader(Error(header)) |> ignore;
    instance.view.setBody(Plain(body)) |> ignore;
  };
};

module Connection_ = {
  let persist = (instance, conn) => instance.connection = Some(conn);

  // connect if not connected yet
  let establish = (instance): Promise.t(result(Connection.t, Error.t)) => {
    switch (instance.connection) {
    | None =>
      Connection.make()
      ->Promise.mapError(e => Error.Connection(e))
      ->Promise.tapOk(persist(instance))
    | Some(connection) => Promise.resolved(Ok(connection))
    };
  };

  let destroy = instance => {
    switch (instance.connection) {
    | None => ()
    | Some(connection) =>
      connection |> Connection.disconnect |> ignore;
      instance.connection = None;
    };
  };

  let sendRequest =
      (request, instance): Promise.t(result(GCL__Response.t, Error.t)) => {
    let value = GCL.Request.encode(request);
    Js.log2("<<<", value);

    let%Ok conn = instance->establish;
    let%Ok result =
      Connection.send(value, conn)
      ->Promise.mapError(e => Error.Connection(e));

    Js.log2(">>>", result);

    // catching exceptions occured when decoding JSON values
    switch (GCL__Response.decode(result)) {
    | value => Promise.resolved(Ok(value))
    | exception (Json.Decode.DecodeError(msg)) =>
      Promise.resolved(Error(Error.Decode(msg, result)))
    };
  };
};

let destroy = instance => {
  instance.decorations |> Array.forEach(Atom.Decoration.destroy);
  instance.connection |> Option.forEach(Connection.disconnect >> ignore);
  instance |> View.destroy;
};
