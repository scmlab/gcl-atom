// open Type.View;
open Rebase;
open React;

module ProofObligation = {
  type t =
    | ProofObligation(int, Pred.t);

  [@react.component]
  let make = (~payload: t) => {
    let ProofObligation(i, pred) = payload;
    <li className="gcl-proof-obligation-item">
      <span> {string(string_of_int(i))} </span>
      <span> {string(Pred.toString(pred))} </span>
    </li>;
  };
};

type t =
  | Nothing
  | ProofObligations(array(ProofObligation.t))
  | Plain(string);

[@react.component]
let make = (~body: t) => {
  switch (body) {
  | Nothing => <> </>
  | ProofObligations([||]) => <> </>
  | ProofObligations(ps) =>
    let list =
      ps
      |> Array.map(payload => <ProofObligation payload />)
      |> Util.React.manyIn(
           "ul",
           ~props=
             ReactDOMRe.domProps(~className="gcl-proof-obligation-list", ()),
         );
    <div className="gcl-body"> list </div>;

  | Plain(s) =>
    let paragraphs =
      s
      |> Js.String.split("\n")
      |> Array.filter(x => !String.isEmpty(x))
      |> Array.map(s => <p> {string(s)} </p>)
      |> Util.React.manyIn(
           "div",
           ~props=ReactDOMRe.domProps(~className="gcl-plain-text", ()),
         );
    <div className="gcl-body"> paragraphs </div>;
  };
};
