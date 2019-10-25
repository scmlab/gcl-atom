open Async;

module Event = Event;

type t = {
  editor: Atom.TextEditor.t,
  mutable connection: option(Connection.t),
};

let make = (editor: Atom.TextEditor.t) => {
  Js.log2("[ instance ][ construct ]", editor |> Atom.TextEditor.getPath);
  // add "gcl" to the class-list
  editor
  |> Atom.Views.getView
  |> Webapi.Dom.HtmlElement.classList
  |> Webapi.Dom.DomTokenList.add("gcl");

  {editor, connection: None};
};

let destroy = self => {
  Js.log2("[ instance ][ destroy ]", self.editor |> Atom.TextEditor.getPath);
  // remove "gcl" to the class-list
  self.editor
  |> Atom.Views.getView
  |> Webapi.Dom.HtmlElement.classList
  |> Webapi.Dom.DomTokenList.remove("gcl");

  ();
};

let dispatch = (command, self) => {
  Command.(
    switch (command) {
    | Activate =>
      Js.log("[ activated ]");
      Connection.autoSearch("gcl")
      |> thenOk(Connection.connect)
      |> thenOk(connection => {
           self.connection = Some(connection);
           Js.log("[ connected ]");
           resolve();
         })
      |> finalError(error =>
           Js.log2("[ connection error ]", Connection.Error.toString(error))
         );

    | Deactivate => Js.log("[ deactivate ]")
    | Save =>
      Js.log("[ saved ]");
      self.editor
      |> Atom.TextEditor.save
      |> fromPromise
      |> mapError(_ => ())
      |> thenOk(_ => {
           Js.log("[ sending ]");
           switch (self.connection) {
           | Some(connection) =>
             Connection.send("{\"tag\": \"Hey\"}", connection)
           | None =>
             Js.log("[ send failed ]");
             resolve("");
           };
         })
      |> finalOk(result => Js.log2("[ received ]", result));
    }
  );
};

let activate = _ => ();

let deactivate = _ => ();
