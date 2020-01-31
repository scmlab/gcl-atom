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
  // <Marker kind=If text="guards">
  //   {guards
  //    |> Array.map(x => <Self value={Pred(x)} />)
  //    |> Util.React.sepBy(string({j| ∨ |j}))}
  // </Marker>
  // | IfBranchConj(value, expr) =>
  //   <Marker kind=If text="invariant">
  //     <Self value />
  //     {string({j| ∧ |j})}
  //     <Self value={Pred(expr)} />
  //   </Marker>
  // | LoopTermDecrConj(value, x, y) =>
  //   <Marker kind=Loop text="bound & invariant">
  //     <Self value />
  //     {string({j| ∧ |j})}
  //     <Self value={Pred(x)} />
  //     {string({j| ∧ |j})}
  //     <Self value={Pred(y)} />
  //   </Marker>
  // | LoopTermConj(value, guards) =>
  //   <Marker kind=Loop text="invariant & guards">
  //     <Self value />
  //     {string({j| ∧ |j})}
  //     {guards
  //      |> Array.map(x => <Self value={Pred(x)} />)
  //      |> Util.React.sepBy(string({j| ∨ |j}))}
  //   </Marker>
  // | LoopIndConj(value, expr) =>
  //   <Marker kind=Loop text="invariant">
  //     <Self value />
  //     {string({j| ∧ |j})}
  //     <Self value={Pred(expr)} />
  //   </Marker>
  // | LoopBaseConj(value, guards) =>
  //   <Marker kind=Loop text="invariant & guards">
  //     <Self value />
  //     {string({j| ∧ |j})}
  //     {guards
  //      |> Array.map(x => <Self value={Pred(x)} />)
  //      |> Util.React.sepBy(string({j| ∨ |j}))}
  //   </Marker>
  };
};
