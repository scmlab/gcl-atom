open Type.View;
open React;

module ProofObligation = {
  type t =
    | ProofObligation(int, Js.Json.t);

  [@react.component]
  let make = (~po: t) => {
    let ProofObligation(i, p) = po;
    <div className="gcl-proof-obligation">
      <span> {string(string_of_int(i))} </span>
      <span> {string(Js.Json.stringify(p))} </span>
    </div>;
  };
};

[@react.component]
let make = (~body: body) => {
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
};
