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
