open Async;

module Event = Event;

type t = {
  editor: Atom.TextEditor.t,
  mutable connection: Connection.t,
};

let make = (editor: Atom.TextEditor.t) => {
  Js.log2("[ instance ][ construct ]", editor |> Atom.TextEditor.getPath);
  // add "gcl" to the class-list
  editor
  |> Atom.Views.getView
  |> Webapi.Dom.HtmlElement.classList
  |> Webapi.Dom.DomTokenList.add("gcl");

  {
    editor,
    connection: {
      path: None,
      process: None,
    },
  };
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

let isConnected = self =>
  switch (self.connection.process) {
  | None => false
  | Some(_) => true
  };

let dispatch = (command, self) => {
  Command.(
    switch (command) {
    | Activate =>
      if (isConnected(self)) {
        ();
      } else {
        Connection.autoSearch("gcl")
        |> thenOk(Connection.make)
        |> thenOk(connection => {
             self.connection = connection;
             Js.log("[ connected ]");
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
      self.connection = Connection.disconnect(self.connection);
      Js.log("[ deactivate ]");
    | Save =>
      Js.log("[ saved ]");
      self.editor
      |> Atom.TextEditor.save
      |> fromPromise
      |> mapError(_ => ())
      |> thenOk(_ => {
           Js.log("[ sending ]");
           let filepath = Atom.TextEditor.getPath(self.editor);
           switch (filepath) {
           | Some(path) =>
             Connection.send(
               "{\"tag\": \"Load\", \"contents\": \"" ++ path ++ "\"}",
               self.connection,
             )
           | None => reject()
           };
         })
      |> finalOk(result => Js.log2("[ received ]", result));
    }
  );
};

let activate = _ => ();

let deactivate = _ => ();
