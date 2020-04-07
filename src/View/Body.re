// open Type.View;
open Belt;
open React;

open Guacamole.GCL.Response;

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
let make = (~body: Guacamole.View.Request.body) => {
  switch (body) {
  | Nothing => <> </>
  | ProofObligations([||]) => <> </>
  | ProofObligations(ps) =>
    let list =
      ps
      ->Array.mapWithIndex((i, payload) =>
          <ProofObligation payload key={string_of_int(i)} />
        )
      ->React.array;
    <div className="gcl-body">
      <ul className="gcl-proof-obligation-list"> list </ul>
    </div>;

  | Plain(s) =>
    let paragraphs =
      s
      ->Js.String.split("\n")
      ->Array.keep(x => x !== "")
      ->Array.mapWithIndex((i, s) =>
          <p key={string_of_int(i)}> {string(s)} </p>
        )
      ->React.array;
    <div className="gcl-body">
      <div className="gcl-plain-text gcl-body-item"> paragraphs </div>
    </div>;
  };
};