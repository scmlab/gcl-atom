open Type.View;

[@react.component]
let make = (~channels: Channels.t) => {
  open React;
  let (header, setHeader) = Hook.useState(AllGood);
  let (body, setBody) = Hook.useState(Nothing);
  let (activated, setActivation) = Hook.useState(false);

  Hook.useChannel(x => x |> setHeader |> Async.resolve, channels.setHeader);
  Hook.useChannel(x => x |> setBody |> Async.resolve, channels.setBody);
  Hook.useChannel(
    x => x |> setActivation |> Async.resolve,
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

  let bodyElem =
    switch (body) {
    | Nothing => <> </>
    | Plain(s) =>
      let paragraphs =
        s
        |> Js.String.split("\n")
        |> Array.map(s => <p> {string(s)} </p>)
        |> Util.React.manyIn("div");
      <div className="gcl-body"> paragraphs </div>;
    };

  <section className={activated ? "" : "hidden"}>
    headerElem
    bodyElem
  </section>;
};
