// open Type.View;
open Rebase;
open React;

open Response;

type t =
  | Nothing
  | ProofObligations(array(ProofObligation.t))
  | Plain(string);

module ProofObligation = {
  [@react.component]
  let make = (~payload: ProofObligation.t) =>
    switch (payload) {
    | ProofObligation(_, p, q, o) =>
      let origin =
        <Link loc={Origin.locOf(o)}> {string(Origin.toString(o))} </Link>;

      <li className="gcl-body-item native-key-bindings" tabIndex=(-1)>
        <span className="gcl-proof-obligation-message"> origin </span>
        <span className="gcl-proof-obligation-antecedent">
          <Pred value=p />
        </span>
        <span className="gcl-proof-obligation-arrow">
          {string({j|⇒|j})}
        </span>
        <span className="gcl-proof-obligation-consequent">
          <Pred value=q />
        </span>
      </li>;
    };
};

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
           ~props=
             ReactDOMRe.domProps(
               ~className="gcl-plain-text gcl-body-item",
               (),
             ),
         );
    <div className="gcl-body"> paragraphs </div>;
  };
};
