module Impl: Guacamole.Sig.State =
  (Editor: Guacamole.Sig.Editor) => {
    type editor = Editor.editor;
    type context = Editor.context;

    type t = {
      editor,
      view: Editor.view,
      mutable mode: Guacamole.View.Response.mode,
      mutable specifications: array(Guacamole.GCL.Response.Specification.t),
      mutable connection: option(Guacamole.Connection.t),
    };

    // getters
    let getEditor = state => state.editor;
    let setSpecifications = (state, specifications) =>
      state.specifications = specifications;

    // connect if not connected yet
    let connect = state =>
      switch (state.connection) {
      | None =>
        Guacamole.Connection.make(
          Editor.Config.getGCLPath,
          Editor.Config.setGCLPath,
        )
        ->Promise.mapError(e => Guacamole.Sig.Error.Connection(e))
        ->Promise.tapOk(conn => state.connection = Some(conn))
      | Some(connection) => Promise.resolved(Ok(connection))
      };
    let disconnect = state =>
      switch (state.connection) {
      | None => Promise.resolved()
      | Some(connection) => Guacamole.Connection.disconnect(connection)
      };
    let sendRequest = (state, request) => {
      let value = Guacamole.Types.Request.encode(request);
      Js.log2("<<<", value);

      let%Ok conn = state->connect;
      let%Ok result =
        Guacamole.Connection.send(value, conn)
        ->Promise.mapError(e => Guacamole.Sig.Error.Connection(e));

      Js.log2(">>>", result);

      // catching exceptions occured when decoding JSON values
      switch (Guacamole.GCL.Response.decode(result)) {
      | value => Promise.resolved(Ok(value))
      | exception (Json.Decode.DecodeError(msg)) =>
        Promise.resolved(Error(Guacamole.Sig.Error.Decode(msg, result)))
      };
    };

    let make = (context, editor) => {
      // view initialization
      let view = Editor.View.make(context, editor);

      let state = {
        editor,
        view,
        mode: WP1,
        specifications: [||],
        connection: None,
      };
      // on view receiving message
      Guacamole.View.Response.(
        view->Editor.View.recv(
          fun
          | SetMode(WP1) => state.mode = WP1
          | SetMode(WP2) => state.mode = WP2
          | Link(ev) => Js.log2("[ view ][ recv ][ link ]", ev),
        )
      )
      ->Editor.addToSubscriptions(context);
      // connection initialization
      state
      ->connect
      ->Promise.get(
          fun
          | Error(e) =>
            Js.log2("[ connection error ]", Guacamole.Sig.Error.toString(e))
          | Ok(c) => Js.log2("[ connection success ]", c),
        );

      state;
    };

    let destroy = state => {
      state.view->Editor.View.destroy;
      state->disconnect;
    };

    //
    // View-related
    //
    let show = state => state.view->Editor.View.show;
    let hide = state => state.view->Editor.View.hide;
    let display = (state, header, body) =>
      state.view
      ->Editor.View.send(Guacamole.View.Request.Display(header, body));
  };