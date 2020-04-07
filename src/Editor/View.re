open Belt;
open Base;
open Types.View;

module PanelContainer = {
  // get "article.gcl-panel-container", create one if not found
  external fromDomElement: Dom.element => Atom.Workspace.item = "%identity";
  external asElement:
    Webapi.Dom.HtmlElement.t_htmlElement => Webapi.Dom.Element.t =
    "%identity";
  open Webapi.Dom;

  let make = (): Element.t => {
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

    // see if the container has already been created
    let containers =
      Atom.Workspace.getBottomPanels()
      ->Array.map(Atom.Views.getView)
      ->Array.map(xs =>
          xs
          ->HtmlElement.childNodes
          ->NodeList.toArray
          ->Array.keepMap(HtmlElement.ofNode)
        )
      ->Array.concatMany
      ->Array.keep(elem =>
          HtmlElement.className(elem) == "gcl-panel-container"
        );

    switch (containers[0]) {
    | None => createBottomPanelContainer()
    | Some(container) => asElement(container)
    };
  };

  let add = (container, element) =>
    container |> Element.appendChild(element);
};

let make = (editor: Atom.TextEditor.t) => {
  open Webapi.Dom;
  let container = PanelContainer.make();

  // add "gcl" to the class-list
  editor
  |> Atom.Views.getView
  |> HtmlElement.classList
  |> DomTokenList.add("gcl");

  // create a element to house the panel
  let element = document |> Document.createElement("article");
  element |> Element.setAttribute("tabIndex", "-1");
  element |> Element.classList |> DomTokenList.add("gcl-panel");
  element |> Element.classList |> DomTokenList.add("native-key-bindings");

  // add the element to the container
  PanelContainer.add(container, element);

  // channels for communicating with the view
  let channels = Channels.make();
  let events = Events.make();
  // render
  let component =
    React.createElementVariadic(
      Panel.make,
      Panel.makeProps(~channels, ~events, ()),
      [||],
    );
  ReactDOMRe.render(component, element);
  // <Links>
  let linkDict: Js.Dict.t(Atom.Decoration.t) = Js.Dict.empty();
  let delete_: string => unit = [%raw "function (id) {delete linkDict[id]}"];
  open Link;
  events.onLink.on(
    fun
    | MouseOver(loc) => {
        let key = Loc.toString(loc);
        Js.Dict.set(linkDict, key, Decoration.markLink(loc, editor));
      }
    | MouseOut(loc) => {
        let key = Loc.toString(loc);
        Js.Dict.get(linkDict, key)->Option.forEach(Atom.Decoration.destroy);
        delete_(key);
      }
    | MouseClick(loc) => {
        let range = Loc.toRange(loc);
        // select the range
        Atom.TextEditor.setSelectedScreenRange(range, editor);
      },
  )
  |> ignore;

  // expose the interface
  Types.View.make(editor, element, channels, events);
};

//
// View
//
module Impl = (Editor: Guacamole.Sig.Editor) => {
  let make = (_, editor) => {
    let view = make(editor);
    // show the panel
    view->send(Show);
    view;
  };
  let destroy = (view: t) => {
    open Webapi.Dom;

    // unmount the component
    ReactDOMRe.unmountComponentAtNode(view.element);
    Element.remove(view.element);

    // remove "gcl" from the class-list of the editor
    view.editor
    |> Atom.Views.getView
    |> Webapi.Dom.HtmlElement.classList
    |> Webapi.Dom.DomTokenList.remove("gcl");
  };

  let show = view => view->send(Show);
  let hide = view => view->send(Hide);
};