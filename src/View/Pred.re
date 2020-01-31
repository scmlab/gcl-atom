// open Rebase;
open React;
open Base;

type kind =
  | Default
  | Guard
  | Assertion
  | If
  | Loop;

module Marker = {
  [@react.component]
  let make = (~kind=Default, ~text=?, ~loc=Loc.NoLoc, ~children) => {
    let className =
      switch (kind) {
      | Default => ""
      | Guard => " marker-guard"
      | Assertion => " marker-assertion"
      | If => " marker-if"
      | Loop => " marker-loop"
      };
    <div className="marker">
      <div className="marker-content"> children </div>
      <Link loc>
        <div className={"marker-line" ++ className} />
        {switch (text) {
         | Some(text) =>
           <div className={"marker-text" ++ className}> {string(text)} </div>
         | None => <> </>
         }}
      </Link>
    </div>;
  };
};

[@react.component]
let rec make = (~value: Syntax.Pred.t) => {
  module Self = {
    let make = make;
    let makeProps = makeProps;
  };
  switch (value) {
  | Pred(expr) => <Marker> <Expr value=expr /> </Marker>
  | Assertion(expr, loc) =>
    <Marker kind=Assertion loc> <Expr value=expr /> </Marker>
  | Guard(expr, loc) => <Marker kind=Guard loc> <Expr value=expr /> </Marker>
  | Conjunct(predicates) =>
    predicates
    |> Array.map(x => <Self value=x />)
    |> Util.React.sepBy(string({j| ∧ |j}))
  | Disjunct(predicates) =>
    predicates
    |> Array.map(x => <Self value=x />)
    |> Util.React.sepBy(string({j| ∨ |j}))
  | Negate(predicate) => <> {string({j|¬ |j})} <Self value=predicate /> </>
  };
};
