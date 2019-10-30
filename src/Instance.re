open Rebase;
open Async;
open Type;

module Event = Event;

let make = (editor: Atom.TextEditor.t): Type.instance => {
  // add "gcl" to the class-list
  editor
  |> Atom.Views.getView
  |> Webapi.Dom.HtmlElement.classList
  |> Webapi.Dom.DomTokenList.add("gcl");

  let view = View.create(editor);
  let connection = Connection.make();

  {editor, view, connection, decorations: [||]};
};

let destroy = instance => {
  // remove "gcl" from the class-list of the editor
  instance.editor
  |> Atom.Views.getView
  |> Webapi.Dom.HtmlElement.classList
  |> Webapi.Dom.DomTokenList.remove("gcl");
  // destroy the connection
  Connection.disconnect(instance.connection) |> ignore;
  // destroy all decorations
  instance.decorations |> Array.forEach(Atom.Decoration.destroy);
  // destroy the view
  instance.view.destroy();
};

let activate = instance => instance.view.setActivation(true);

let deactivate = instance => instance.view.setActivation(false);

let dispatch = (request, instance) => {
  Command.(
    switch (request) {
    | Activate =>
      activate(instance) |> ignore;

      if (Connection.isConnected(instance.connection)) {
        ();
      } else {
        Connection.connect(instance.connection)
        |> thenOk(_ => resolve())
        |> finalError(error => {
             let (header, body) = Connection.Error.toString(error);
             instance.view.setHeader(header) |> ignore;
             instance.view.setBody(body) |> ignore;
           });
      };

    | Deactivate =>
      instance.connection |> Connection.disconnect;
      deactivate(instance) |> ignore;
      Js.log("[ deactivate ]");
    | Save =>
      instance.decorations |> Array.forEach(Atom.Decoration.destroy);
      instance.editor
      |> Atom.TextEditor.save
      |> fromPromise
      |> mapError(_ => ())
      |> thenOk(_ => {
           let filepath = Atom.TextEditor.getPath(instance.editor);

           switch (filepath) {
           | Some(path) =>
             Connection.send(
               Request.encode(Request.Load(path)),
               instance.connection,
             )
             |> Async.mapError(error => {
                  let (header, body) = Connection.Error.toString(error);
                  instance.view.setHeader(header) |> ignore;
                  instance.view.setBody(body) |> ignore;
                  ();
                })
           | None =>
             instance.view.setHeader("Cannot read filepath ") |> ignore;
             instance.view.setBody("Please save the file first") |> ignore;
             reject();
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
