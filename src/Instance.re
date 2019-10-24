open Async;

module Event = Event;

type t = {editor: Atom.TextEditor.t};

let make = (editor: Atom.TextEditor.t) => {
  Js.log2("[ instance ][ construct ]", editor |> Atom.TextEditor.getPath);
  // add "gcl" to the class-list
  editor
  |> Atom.Views.getView
  |> Webapi.Dom.HtmlElement.classList
  |> Webapi.Dom.DomTokenList.add("gcl");

  {editor: editor};
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
    | Activate => Js.log("[ activate ]")
    | Deactivate => Js.log("[ deactivate ]")
    | Save =>
      self.editor
      |> Atom.TextEditor.save
      |> Async.fromPromise
      |> Async.finalOk(_ => Js.log("[ save ]"))
    }
  );
};

let activate = _ => ();

let deactivate = _ => ();
