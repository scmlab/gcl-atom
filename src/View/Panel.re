open! Types.View;

[@react.component]
let make = (~channels: Channels.t) => {
  open React;
  let (header, setHeader) = Hook.useState(AllGood);
  let (body, setBody) = Hook.useState(Body.Nothing);
  let (activated, setActivation) = Hook.useState(false);

  Hook.useChannel(
    x => x |> setHeader |> Promise.resolved,
    channels.setHeader,
  );
  Hook.useChannel(x => x |> setBody |> Promise.resolved, channels.setBody);
  Hook.useChannel(
    x => x |> setActivation |> Promise.resolved,
    channels.setActivation,
  );

  let headerElem =
    switch (header) {
    | AllGood =>
      <h2 className="gcl-header">
        <div className="text-success"> {string("All Good")} </div>
      </h2>
    | Plain(s) => <h2 className="gcl-header"> <div> {string(s)} </div> </h2>
    | Error(s) =>
      <h2 className="gcl-header">
        <div className="text-error"> {string(s)} </div>
      </h2>
    };

  <section className={activated ? "" : "hidden"}>
    headerElem
    <Body body />
  </section>;
};
