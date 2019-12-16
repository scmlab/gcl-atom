// open Type.View;
open Rebase;
open React;

module ProofObligation = {
  type t =
    | ProofObligation(int, Expr.t, Expr.t);

  [@react.component]
  let make = (~payload: t) => {
    let ProofObligation(_, p, q) = payload;
    <li className="gcl-body-item">
      <span className="gcl-proof-obligation-antecedent">
        {string(Expr.toString(p))}
      </span>
      <span className="gcl-proof-obligation-arrow">
        {string({j|⇒|j})}
      </span>
      <span className="gcl-proof-obligation-consequent">
        {string(Expr.toString(q))}
      </span>
    </li>;
  };

  open Json.Decode;
  let decode: decoder(t) =
    tuple3(int, Expr.decode, Expr.decode)
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
