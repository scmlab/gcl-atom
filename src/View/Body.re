// open Type.View;
open Rebase;
open React;

module ProofObligation = {
  type t =
    | ProofObligation(int, Pred.t, Pred.t);

  [@react.component]
  let make = (~payload: t) => {
    let ProofObligation(_, p, q) = payload;
    <li className="gcl-body-item">
      <span className="gcl-proof-obligation-antecedent">
        {string(Pred.toString(p))}
      </span>
      <span className="gcl-proof-obligation-arrow">
        {string({j|⇒|j})}
      </span>
      <span className="gcl-proof-obligation-consequent">
        {string(Pred.toString(q))}
      </span>
    </li>;
  };

  open Json.Decode;
  let decode: decoder(t) =
    tuple3(int, Pred.decode, Pred.decode)
    |> map(((i, p, q)) => ProofObligation(i, p, q));
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
           ~props=
             ReactDOMRe.domProps(
               ~className="gcl-plain-text gcl-body-item",
               (),
             ),
         );
    <div className="gcl-body"> paragraphs </div>;
  };
};
