open! Types.View;
open Guacamole.View.Response;
open! Guacamole.View.Request;

[@react.component]
let make = (~channels: Channels.t, ~events: Events.t) => {
  open React;
  let (header, setHeader) = Hook.useState(Loading);
  let (body, setBody) = Hook.useState(Nothing);
  let (activated, setActivation) = Hook.useState(false);
  let (mode, setMode) = Hook.useState(WP1);

  Hook.useChannel(
    x => x |> setHeader |> Promise.resolved,
    channels.setHeader,
  );
  Hook.useChannel(x => x |> setBody |> Promise.resolved, channels.setBody);
  Hook.useChannel(
    x => x |> setActivation |> Promise.resolved,
    channels.setActivation,
  );

  let onChange = _ => {
    let newMode =
      switch (mode) {
      | WP1 => WP2
      | WP2 => WP1
      };
    events.onSetMode.emit(newMode);
    setMode(newMode);
  };

  let headerElem = {
    <h2 className="gcl-header">
      {switch (header) {
       | Loading =>
         <div className="text-plain"> {string("Loading ...")} </div>
       | Plain(s) => <div> {string(s)} </div>
       | Error(s) => <div className="text-error"> {string(s)} </div>
       }}
      <div className="gcl-mode">
        <label className="input-label">
          <input
            className="input-toggle"
            type_="checkbox"
            checked={
              switch (mode) {
              | WP1 => false
              | WP2 => true
              }
            }
            onChange
          />
          {string("WP2")}
        </label>
      </div>
    </h2>;
  };

  <Link.Provider value={events.onLink}>
    <section className={activated ? "" : "hidden"}>
      headerElem
      <Body body />
    </section>
  </Link.Provider>;
};