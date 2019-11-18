open Rebase;
open Async;

module Event = Event;

let make = (editor: Atom.TextEditor.t): Type.instance => {
  // add "gcl" to the class-list
  editor
  |> Atom.Views.getView
  |> Webapi.Dom.HtmlElement.classList
  |> Webapi.Dom.DomTokenList.add("gcl");

  let view = View.make(editor);
  let connection = Connection.make();

  {editor, view, connection, decorations: [||]};
};

let destroy = instance => {
  // remove "gcl" from the class-list of the editor
  instance.Type.editor
  |> Atom.Views.getView
  |> Webapi.Dom.HtmlElement.classList
  |> Webapi.Dom.DomTokenList.remove("gcl");
  // destroy the connection
  Connection.disconnect(instance.connection) |> ignore;
  // destroy all decorations
  instance.decorations |> Array.forEach(Atom.Decoration.destroy);
  // destroy the view
  instance.editor |> View.destroy;
};

let activate = instance => instance.Type.view.setActivation(true);

let deactivate = instance => instance.Type.view.setActivation(false);

let rec dispatch = (request, instance) => {
  Command.(
    switch (request) {
    | Activate =>
      activate(instance) |> ignore;
      if (Connection.isConnected(instance.connection)) {
        resolve();
      } else {
        Connection.connect(instance.connection)
        |> thenError(error => {
             let (header, body) = Connection.Error.toString(error);
             instance.view.setHeader(Error(header)) |> ignore;
             instance.view.setBody(Plain(body)) |> ignore;
             resolve();
           })
        |> thenOk(_ => dispatch(Save, instance));
      };
    | Deactivate => deactivate(instance)
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
                  instance.view.setHeader(Error(header)) |> ignore;
                  instance.view.setBody(Plain(body)) |> ignore;
                  ();
                })
           | None =>
             instance.view.setHeader(Error("Cannot read filepath "))
             |> ignore;
             instance.view.setBody(Plain("Please save the file first"))
             |> ignore;
             reject();
           };
         })
      |> thenOk(result => {
           Js.log2("[ received json ]", result);
           Js.log2("[ received value ]", result |> Response.parse);
           Response.parse(result) |> Handler.handle(instance);
         });
    }
  );
};
