// open Rebase;
open React;
// open Base;

type kind =
  | Default
  | If
  | Loop;

module Group = {
  [@react.component]
  let make = (~kind=Default, ~children) => {
    <div className="group">
      <div className="group-content"> children </div>
      {switch (kind) {
       | Default => <div className="group-underline" />
       | If => <div className="group-underline group-if" />
       | Loop => <div className="group-underline group-loop" />
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
  | Pred(expr) => <Group> <Expr value=expr /> </Group>
  | IfTotalDisj(guards) =>
    <Group kind=If>
      {guards
       |> Array.map(x => <Self value={Pred(x)} />)
       |> Util.React.sepBy(string({j| ∨ |j}))}
    </Group>
  | IfBranchConj(value, expr) =>
    <Group kind=If>
      <Self value />
      {string({j| ∧ |j})}
      <Self value={Pred(expr)} />
    </Group>
  | LoopTermDecrConj(value, x, y) =>
    <Group kind=Loop>
      <Self value />
      {string({j| ∧ |j})}
      <Self value={Pred(x)} />
      {string({j| ∧ |j})}
      <Self value={Pred(y)} />
    </Group>
  | LoopTermConj(value, guards) =>
    <Group kind=Loop>
      <Self value />
      {string({j| ∧ |j})}
      {guards
       |> Array.map(x => <Self value={Pred(x)} />)
       |> Util.React.sepBy(string({j| ∨ |j}))}
    </Group>
  | LoopIndConj(value, expr) =>
    <Group kind=Loop>
      <Self value />
      {string({j| ∧ |j})}
      <Self value={Pred(expr)} />
    </Group>
  | LoopBaseConj(value, guards) =>
    <Group kind=Loop>
      <Self value />
      {string({j| ∧ |j})}
      {guards
       |> Array.map(x => <Self value={Pred(x)} />)
       |> Util.React.sepBy(string({j| ∨ |j}))}
    </Group>
  };
};
