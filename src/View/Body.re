// open Type.View;
open Rebase;
open React;

module Origin = {
  open Util;
  type t =
    | AroundAbort(range)
    | AroundSkip(range)
    | AssertGuaranteed(range)
    | AssertSufficient(range)
    | Assignment(range)
    | IfTotal(range)
    | IfBranch(range)
    | LoopBase(range)
    | LoopInd(range)
    | LoopTermBase(range)
    | LoopTermDec(range)
    | LoopInitialize(range);

  open Decoder;
  open! Json.Decode;
  let decode: decoder(t) =
    sum(
      fun
      | "AroundAbort" => Contents(range |> map(x => AroundAbort(x)))
      | "AroundSkip" => Contents(range |> map(x => AroundSkip(x)))
      | "AssertGuaranteed" =>
        Contents(range |> map(x => AssertGuaranteed(x)))
      | "AssertSufficient" =>
        Contents(range |> map(x => AssertSufficient(x)))
      | "Assignment" => Contents(range |> map(x => Assignment(x)))
      | "IfTotal" => Contents(range |> map(x => IfTotal(x)))
      | "IfBranch" => Contents(range |> map(x => IfBranch(x)))
      | "LoopBase" => Contents(range |> map(x => LoopBase(x)))
      | "LoopInd" => Contents(range |> map(x => LoopInd(x)))
      | "LoopTermBase" => Contents(range |> map(x => LoopTermBase(x)))
      | "LoopTermDec" => Contents(range |> map(x => LoopTermDec(x)))
      | "LoopInitialize" => Contents(range |> map(x => LoopInitialize(x)))
      | tag => raise(DecodeError("Unknown constructor: " ++ tag)),
    );
};

module ProofObligation = {
  type t =
    | ProofObligation(int, Expr.t, Expr.t, array(Origin.t));

  [@react.component]
  let make = (~payload: t) => {
    let ProofObligation(_, p, q, _) = payload;
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

  open! Json.Decode;
  let decode: decoder(t) =
    tuple4(int, Expr.decode, Expr.decode, array(Origin.decode))
    |> map(((i, p, q, o)) => ProofObligation(i, p, q, o));
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
