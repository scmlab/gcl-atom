module Impl: Guacamole.State.Sig = (Editor: Guacamole.Sig.Editor) => {
    type editor = Editor.editor;
    type context = Editor.context;

    type t = {
      editor,
      view: Editor.view,
      mutable mode: Types.View.mode,
      mutable connection: option(Guacamole.Connection.t),
    };

    // getters
    let getEditor = state => state.editor;

    // connect if not connected yet
    let connect = state =>
      switch (state.connection) {
      | None =>
        Guacamole.Connection.make(Editor.Config.getGCLPath, Editor.Config.setGCLPath)
        ->Promise.mapError(e => Guacamole.State.Error.Connection(e))
        ->Promise.tapOk(conn => state.connection = Some(conn))
      | Some(connection) => Promise.resolved(Ok(connection))
      };
    let disconnect = state =>
      switch (state.connection) {
      | None => Promise.resolved()
      | Some(connection) => Guacamole.Connection.disconnect(connection)
      };

    let make = (context, editor) => {

      // view initialization
      let view = Editor.View.make(context, editor);

      let state = {
        editor,
        view,
        mode: WP1,
        connection: None,
      };

      // connection initialization
      state
      ->connect
      ->Promise.get(
          fun
          | Error(e) => Js.log2("[ connection error ]", Guacamole.State.Error.toString(e))
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
    let show = state => state.view->Editor.View.show
    let hide = state => state.view->Editor.View.hide
  };