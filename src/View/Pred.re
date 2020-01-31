// open Rebase;
open React;
// open Base;

type kind =
  | Default
  | Guard
  | If
  | Loop;

module Marker = {
  [@react.component]
  let make = (~kind=Default, ~text=?, ~children) => {
    let className =
      switch (kind) {
      | Default => ""
      | Guard => " marker-guard"
      | If => " marker-if"
      | Loop => " marker-loop"
      };

    <div className="marker">
      <div className="marker-content"> children </div>
      <div className={"marker-line" ++ className} />
      {switch (text) {
       | Some(text) =>
         <div className={"marker-text" ++ className}> {string(text)} </div>
       | None => <> </>
       }}
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
  | GuardDisj(guards) =>
    guards
    |> Array.map(x => <Marker kind=Guard> <Expr value=x /> </Marker>)
    |> Util.React.sepBy(string({j| ∨ |j}))
  // <Marker kind=If text="guards">
  //   {guards
  //    |> Array.map(x => <Self value={Pred(x)} />)
  //    |> Util.React.sepBy(string({j| ∨ |j}))}
  // </Marker>
  | IfBranchConj(value, expr) =>
    <Marker kind=If text="invariant">
      <Self value />
      {string({j| ∧ |j})}
      <Self value={Pred(expr)} />
    </Marker>
  | LoopTermDecrConj(value, x, y) =>
    <Marker kind=Loop text="bound & invariant">
      <Self value />
      {string({j| ∧ |j})}
      <Self value={Pred(x)} />
      {string({j| ∧ |j})}
      <Self value={Pred(y)} />
    </Marker>
  | LoopTermConj(value, guards) =>
    <Marker kind=Loop text="invariant & guards">
      <Self value />
      {string({j| ∧ |j})}
      {guards
       |> Array.map(x => <Self value={Pred(x)} />)
       |> Util.React.sepBy(string({j| ∨ |j}))}
    </Marker>
  | LoopIndConj(value, expr) =>
    <Marker kind=Loop text="invariant">
      <Self value />
      {string({j| ∧ |j})}
      <Self value={Pred(expr)} />
    </Marker>
  | LoopBaseConj(value, guards) =>
    <Marker kind=Loop text="invariant & guards">
      <Self value />
      {string({j| ∧ |j})}
      {guards
       |> Array.map(x => <Self value={Pred(x)} />)
       |> Util.React.sepBy(string({j| ∨ |j}))}
    </Marker>
  };
};
