open Rebase;
// open Type;
open Type.View;

module PanelContainer = {
  // get "article.gcl-panel-container", create one if not found
  external fromDomElement: Dom.element => Atom.Workspace.item = "%identity";
  external asElement:
    Webapi.Dom.HtmlElement.t_htmlElement => Webapi.Dom.Element.t =
    "%identity";

  let make = (): Webapi.Dom.Element.t => {
    open Webapi.Dom;
    open DomTokenList;

    // create "article.gcl-panel-container"
    // shared by all instances, should only be invoked once!
    let createBottomPanelContainer = (): Element.t => {
      let panelContainer = document |> Document.createElement("article");
      panelContainer |> Element.classList |> add("gcl-panel-container");
      Atom.Workspace.addBottomPanel({
        "item": fromDomElement(panelContainer),
        "priority": 0,
        "visible": true,
      })
      |> ignore;
      panelContainer;
    };

    let containers =
      Atom.Workspace.getBottomPanels()
      |> Array.map(Atom.Views.getView)
      |> Array.flatMap(xs =>
           xs
           |> HtmlElement.childNodes
           |> NodeList.toArray
           |> Array.filterMap(HtmlElement.ofNode)
         )
      |> Array.filter(elem =>
           elem |> HtmlElement.className == "gcl-panel-container"
         );

    switch (containers[0]) {
    | None => createBottomPanelContainer()
    | Some(container) => asElement(container)
    };
  };
};

// type body =
//   | Plain(string)
//   | Error(string);

let make = (editor: Atom.TextEditor.t) => {
  open Webapi.Dom;
  let container = PanelContainer.make();

  // create a element to house the panel
  let element = document |> Document.createElement("article");
  element |> Element.classList |> DomTokenList.add("gcl-panel");
  let id = "gcl:" ++ string_of_int(Atom.TextEditor.id(editor));
  Element.setId(element, id);
  container |> Element.appendChild(element);

  // channels for communicating with the view
  let channels = Channels.make();
  // render
  let component =
    React.createElementVariadic(
      Panel.make,
      Panel.makeProps(~channels, ()),
      [||],
    );
  ReactDOMRe.render(component, element);
  // expose the interface
  Interface.make(channels);
};

let destroy = (editor: Atom.TextEditor.t) => {
  open Webapi.Dom;
  let id = "gcl:" ++ string_of_int(Atom.TextEditor.id(editor));
  document
  |> Document.getElementById(id)
  |> Option.forEach(element => {
       ReactDOMRe.unmountComponentAtNode(element);
       Element.remove(element);
     });
};
