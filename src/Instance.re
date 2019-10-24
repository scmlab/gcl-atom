open Async;
module Event = Event;

type t = {editor: Atom.TextEditor.t};

let make = (editor: Atom.TextEditor.t) => {
  Js.log2("make", editor |> Atom.TextEditor.getPath);
  // add "gcl" to the class-list
  editor
  |> Atom.Views.getView
  |> Webapi.Dom.HtmlElement.classList
  |> Webapi.Dom.DomTokenList.add("gcl");

  {editor: editor};
};

let destroy = self => {
  Js.log2("destroy", self.editor |> Atom.TextEditor.getPath);
  // remove "gcl" to the class-list
  self.editor
  |> Atom.Views.getView
  |> Webapi.Dom.HtmlElement.classList
  |> Webapi.Dom.DomTokenList.remove("gcl");

  ();
};

let activate = self => {
  Js.log("activating");
};

let deactivate = self => {
  Js.log("deactivating");
};
