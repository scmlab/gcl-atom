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
    setHeader: Channel.t(string, unit, unit),
    setBody: Channel.t(string, unit, unit),
  };

  let make = () => {
    updateConnection: Channel.make(),
    setHeader: Channel.make(),
    setBody: Channel.make(),
  };
};

[@react.component]
let make = (~channels: Channels.t) => {
  open React;
  let (header, setHeader) = Hook.useState("");
  let (body, setBody) = Hook.useState("");
  Hook.useChannel(x => x |> setHeader |> Async.resolve, channels.setHeader);
  Hook.useChannel(x => x |> setBody |> Async.resolve, channels.setBody);
  Js.log2("[ view ]", header);
  // let ((connection, connectionError), setConnectionAndError) =
  //   Hook.useState((Connection.make(), None));
  //
  // Hook.useChannel(
  //   x => x |> setConnectionAndError |> Async.resolve,
  //   channels.updateConnection,
  // );
  // let connected = Connection.isConnected(connection);
  // let status =
  //   connected
  //     ? <span
  //         title="connected"
  //         id="connection-status"
  //         className="icon icon-primitive-dot text-success"
  //       />
  //     : <span
  //         title="disconnected"
  //         id="connection-status"
  //         className="icon icon-primitive-dot text-error"
  //       />;
  // let (header, body) =
  //   switch (connectionError) {
  //   | None => ("All good", "")
  //   | Some(err) => Connection.Error.toString(err)
  //   };
  <section className="gcl-panel">
    <h2> {string(header)} </h2>
    <div> {string(body)} </div>
  </section>;
};

module Interface = {
  type t = {
    setHeader: string => Async.t(unit, unit),
    setBody: string => Async.t(unit, unit),
  };

  let make = (channels: Channels.t) => {
    setHeader: s => {
      Js.log2("[ channel ]", s);
      channels.setHeader |> Channel.send(s);
    },
    setBody: s => {
      channels.setBody |> Channel.send(s);
    },
  };
};

let create = () => {
  // open Webapi.Dom;
  let element = PanelContainer.make();
  // document |> Document.createElement("article");

  // let handles = View.makeHandles();
  let channels = Channels.make();
  // let view = View.make(handles, channels);

  let component =
    React.createElementVariadic(make, makeProps(~channels, ()), [||]);
  ReactDOMRe.render(component, element);
  // return the interface
  Interface.make(channels);
};
