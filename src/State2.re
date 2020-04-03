module Impl: Guacamole.State.Sig =
  (Editor: Guacamole.Editor.Sig) => {
    type editor = Editor.editor;
    type context = Editor.context;

    type t = {
      editor,
      mutable loaded: bool,
      mutable mode: Types.View.mode,
      mutable connection: option(Guacamole.Connection.t),
    };

    // connect if not connected yet
    let connect = state =>
      switch (state.connection) {
      | None =>
        Guacamole.Connection.make(Editor.getGCLPath, Editor.setGCLPath)
        ->Promise.mapError(e => Guacamole.State.Error.Connection(e))
        ->Promise.tapOk(conn => state.connection = Some(conn))
      | Some(connection) => Promise.resolved(Ok(connection))
      };
    let disconnect = state =>
      switch (state.connection) {
      | None => Promise.resolved()
      | Some(connection) => Guacamole.Connection.disconnect(connection)
      };

    let make = (_, editor) => {
      editor,
      loaded: false,
      mode: WP1,
      connection: None,
    };
    let destroy = state => {
      state.loaded = false;
      state->disconnect;
    };
  };