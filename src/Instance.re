open Rebase;
open Async;
open Type;

module Event = Event;

let make = (editor: Atom.TextEditor.t): Type.instance => {
  Js.log2("[ instance ][ construct ]", editor |> Atom.TextEditor.getPath);
  // add "gcl" to the class-list
  editor
  |> Atom.Views.getView
  |> Webapi.Dom.HtmlElement.classList
  |> Webapi.Dom.DomTokenList.add("gcl");

  {editor, connection: Connection.make(), decorations: [||]};
};

let destroy = instance => {
  Js.log2(
    "[ instance ][ destroy ]",
    instance.editor |> Atom.TextEditor.getPath,
  );
  // remove "gcl" to the class-list
  instance.editor
  |> Atom.Views.getView
  |> Webapi.Dom.HtmlElement.classList
  |> Webapi.Dom.DomTokenList.remove("gcl");
  // destroy the connection
  Connection.disconnect(instance.connection) |> ignore;
  // destroy all decorations
  instance.decorations |> Array.forEach(Atom.Decoration.destroy);
};

let dispatch = (request, instance) => {
  Command.(
    switch (request) {
    | Activate =>
      if (Connection.isConnected(instance.connection)) {
        ();
      } else {
        let channels = View.create();

        Connection.connect(instance.connection)
        |> thenOk(_ => {
             channels.updateConnection
             |> Channel.send((instance.connection, None));
             resolve();
           })
        |> finalError(error =>
             Js.log2(
               "[ connection error ]",
               Connection.Error.toString(error),
             )
           );
      }

    | Deactivate =>
      instance.connection |> Connection.disconnect;
      Js.log("[ deactivate ]");
    | Save =>
      instance.decorations |> Array.forEach(Atom.Decoration.destroy);
      instance.editor
      |> Atom.TextEditor.save
      |> fromPromise
      |> mapError(_ => ())
      |> thenOk(_ => {
           Js.log("[ sending ]");
           let filepath = Atom.TextEditor.getPath(instance.editor);
           switch (filepath) {
           | Some(path) =>
             Connection.send(
               Request.encode(Request.Load(path)),
               instance.connection,
             )
             |> Async.mapError(_ => ())
           | None => reject()
           };
         })
      |> finalOk(result => {
           Js.log2("[ received json ]", result |> Response.test);
           Js.log2("[ received value ]", result |> Response.parse);
           Response.parse(result)
           |> Option.forEach(Handler.handle(instance));
         });
    }
  );
};

let activate = _ => ();

let deactivate = _ => ();
