// open Rebase;
open React;
open Base;

open Syntax.Pred;

type kind =
  | Guard
  | Assertion;

module Marker = {
  [@react.component]
  let make = (~kind=?, ~sort=?, ~text=?, ~loc=Loc.NoLoc, ~children) => {
    let kind =
      switch (kind) {
      | None => ""
      | Some(Guard) => " marker-guard"
      | Some(Assertion) => " marker-assertion"
      };
    let sort =
      switch (sort) {
      | None => ""
      | Some(If(_)) => " marker-if"
      | Some(Loop(_)) => " marker-loop"
      | Some(Bnd) => " marker-bound"
      };
    <div className={"marker" ++ sort ++ kind}>
      <div className="marker-content"> children </div>
      <Link loc>
        <div className={"marker-line" ++ sort ++ kind} />
        {switch (text) {
         | Some(text) =>
           <div className={"marker-text" ++ sort ++ kind}>
             {string(text)}
           </div>
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
  | Constant(expr) => <Marker> <Expr value=expr /> </Marker>
  | Bound(expr, loc) =>
    <Marker sort=Bnd loc text="bound"> <Expr value=expr /> </Marker>
  | Assertion(expr, loc) =>
    <Marker kind=Assertion loc text="assertion"> <Expr value=expr /> </Marker>
  | LoopInvariant(expr, _, loc) =>
    <Marker kind=Assertion sort={Loop(NoLoc)} text="loop invariant" loc>
      <Expr value=expr />
    </Marker>
  | Guard(expr, sort, loc) =>
    <Marker kind=Guard sort loc text="guard"> <Expr value=expr /> </Marker>
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
