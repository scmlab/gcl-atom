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
  };

  let make = () => {updateConnection: Channel.make()};
};

[@react.component]
let make = (~channels: Channels.t) => {
  open React;
  let ((connection, connectionError), setConnectionAndError) =
    Hook.useState((Connection.make(), None));

  Hook.useChannel(
    x => x |> setConnectionAndError |> Async.resolve,
    channels.updateConnection,
  );
  let connected = Connection.isConnected(connection);
  let status =
    connected
      ? <span
          title="connected"
          id="connection-status"
          className="icon icon-primitive-dot text-success"
        />
      : <span
          title="disconnected"
          id="connection-status"
          className="icon icon-primitive-dot text-error"
        />;
  let (header, body) =
    switch (connectionError) {
    | None => ("All good", "")
    | Some(err) => Connection.Error.toString(err)
    };
  <section className="gcl-panel">
    <h2> <span> {string(header)} </span> status </h2>
    <div> <span> {string(body)} </span> </div>
  </section>;
};

let create = () => {
  // Js.log("[ view create ]");

  // open Webapi.Dom;
  let element = PanelContainer.make();
  // document |> Document.createElement("article");

  // let handles = View.makeHandles();
  let channels = Channels.make();
  // let view = View.make(handles, channels);

  let component =
    React.createElementVariadic(make, makeProps(~channels, ()), [||]);
  ReactDOMRe.render(component, element);
  /* return the handles for drilling */
  channels;
};
