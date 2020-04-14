open! Types.View;
open Guacamole.View;

[@react.component]
let make = (~channels: Channels.t, ~events: Events.t) => {
  let (header, setHeader) = Hook.useState(Request.Header.Loading);
  let (body, setBody) = Hook.useState(Request.Body.Nothing);
  let (activated, setActivation) = Hook.useState(false);
  let (mode, setMode) = React.useState(_ => Response.WP1);

  Hook.useChannel(
    x => x |> setHeader |> Promise.resolved,
    channels.setHeader,
  );
  Hook.useChannel(x => x |> setBody |> Promise.resolved, channels.setBody);
  Hook.useChannel(
    x => x |> setActivation |> Promise.resolved,
    channels.setActivation,
  );

  // mode
  let onChangeMode = mode => setMode(_ => mode);
  React.useEffect1(
    () => {
      events.onSetMode.emit(mode);
      None;
    },
    [|mode|],
  );

  <Link.Provider value={events.onLink}>
    <section className={activated ? "" : "hidden"}>
      <Guacamole.Header
        header
        editorType=Guacamole.Sig.Atom
        mode
        onChangeMode
      />
      <Body body />
    </section>
  </Link.Provider>;
};