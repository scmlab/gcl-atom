open Belt;

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

type t = {
  editor: Atom.TextEditor.t,
  element: Webapi.Dom.Element.t,
  subscriptions: array(unit => unit),
  onRequest: Guacamole.Event.t(Guacamole.View.Request.t),
  onResponse: Guacamole.Event.t(Guacamole.View.Response.t),
};

// messaging
let send = (view, request) => {
  view.onRequest.emit(request);
  Promise.resolved(true);
};
let recv = (view, callback) => {
  view.onResponse.on(callback)->Js.Array.push(view.subscriptions)->ignore;
  Atom.Disposable.make(_ => ());
};

// show/hide
let show = view => view->send(Show)->ignore;
let hide = view => view->send(Hide)->ignore;

let make = (_context, editor: Atom.TextEditor.t) => {
  let editorType = Guacamole.Sig.Atom;
  // event emitters for communicating with the view
  let onRequest = Guacamole.Event.make();
  let onResponse = Guacamole.Event.make();
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

  // render
  ReactDOMRe.render(
    <Guacamole.Panel editorType onRequest onResponse />,
    element,
  );

  // <Links>
  // let linkDict: Js.Dict.t(Atom.Decoration.t) = Js.Dict.empty();
  // let delete_: string => unit = [%raw "function (id) {delete linkDict[id]}"];
  // onResponse.on(
  //   fun
  //   | Guacamole.View.Response.Link(MouseOver(loc)) => {
  //       let key = Guacamole.GCL.Loc.toString(loc);
  //       // Js.Dict.set(linkDict, key, Decoration.markLink(loc, editor));
  //       ();
  //     }
  //   | Link(MouseOut(loc)) => {
  //       let key = Guacamole.GCL.Loc.toString(loc);
  //       Js.Dict.get(linkDict, key)->Option.forEach(Atom.Decoration.destroy);
  //       delete_(key);
  //     }
  //   | Link(MouseClick(loc)) => {
  //       // let range = Base2.Loc.toRange(loc);
  //       // select the range
  //       Atom.TextEditor.setSelectedScreenRange(range, editor);
  //     }
  //   | SetMode(_mode) => ()
  //   | Initialized => ()
  //   | Destroyed => (),
  // )
  // |> ignore;

  let view = {editor, element, subscriptions: [||], onRequest, onResponse};

  // show the panel
  view->send(Show)->ignore;

  view;
};

let destroy = view => {
  // unmount the component
  ReactDOMRe.unmountComponentAtNode(view.element);
  Webapi.Dom.Element.remove(view.element);

  // remove "gcl" from the class-list of the editor
  view.editor
  |> Atom.Views.getView
  |> Webapi.Dom.HtmlElement.classList
  |> Webapi.Dom.DomTokenList.remove("gcl");

  view.subscriptions->Belt.Array.forEach(destructor => destructor());
};