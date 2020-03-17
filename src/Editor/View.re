open Belt;
open Base;
open Types.View;

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
  //
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
  // <Links>
  let linkDict: Js.Dict.t(Atom.Decoration.t) = Js.Dict.empty();
  let delete_: string => unit = [%raw "function (id) {delete linkDict[id]}"];
  open Link;
  channels.link.on(
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
  Interface.make(channels);
};

let destroy = (editor: Atom.TextEditor.t) => {
  open Webapi.Dom;

  // unmount the component
  let id = "gcl:" ++ string_of_int(Atom.TextEditor.id(editor));

  Document.getElementById(id, document)
  ->Option.forEach(element => {
      ReactDOMRe.unmountComponentAtNode(element);
      Element.remove(element);
    });

  // remove "gcl" from the class-list of the editor
  editor
  |> Atom.Views.getView
  |> Webapi.Dom.HtmlElement.classList
  |> Webapi.Dom.DomTokenList.remove("gcl");
};