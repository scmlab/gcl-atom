open Rebase;

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

module Channels = {
  type t = {
    updateConnection:
      Channel.t((Connection.t, option(Connection.Error.t)), unit, unit),
    setActivation: Channel.t(bool, unit, unit),
    setHeader: Channel.t(string, unit, unit),
    setBody: Channel.t(string, unit, unit),
  };

  let make = () => {
    updateConnection: Channel.make(),
    setActivation: Channel.make(),
    setHeader: Channel.make(),
    setBody: Channel.make(),
  };
};

[@react.component]
let make = (~channels: Channels.t) => {
  open React;
  let (header, setHeader) = Hook.useState("");
  let (body, setBody) = Hook.useState("");
  let (activated, setActivation) = Hook.useState(false);

  Hook.useChannel(x => x |> setHeader |> Async.resolve, channels.setHeader);
  Hook.useChannel(x => x |> setBody |> Async.resolve, channels.setBody);
  Hook.useChannel(
    x => x |> setActivation |> Async.resolve,
    channels.setActivation,
  );

  <section className={activated ? "" : "hidden"}>
    <h2> {string(header)} </h2>
    <div> {string(body)} </div>
  </section>;
};

module Interface = {
  type t = {
    destroy: unit => unit,
    setActivation: bool => Async.t(unit, unit),
    setHeader: string => Async.t(unit, unit),
    setBody: string => Async.t(unit, unit),
  };

  let make = (editor: Atom.TextEditor.t, channels: Channels.t) => {
    destroy: () => {
      open Webapi.Dom;
      let id = "gcl:" ++ string_of_int(Atom.TextEditor.id(editor));
      document
      |> Document.getElementById(id)
      |> Option.forEach(element => {
           ReactDOMRe.unmountComponentAtNode(element);
           Element.remove(element);
         });
    },
    setActivation: s => {
      channels.setActivation |> Channel.send(s);
    },
    setHeader: s => {
      channels.setHeader |> Channel.send(s);
    },
    setBody: s => {
      channels.setBody |> Channel.send(s);
    },
  };
};

let create = (editor: Atom.TextEditor.t) => {
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
    React.createElementVariadic(make, makeProps(~channels, ()), [||]);
  ReactDOMRe.render(component, element);
  // expose the interface
  Interface.make(editor, channels);
};
