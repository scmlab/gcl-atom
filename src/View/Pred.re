open React;

open Guacamole.GCL.Syntax.Pred;

type kind =
  | Guard
  | Assertion;

type sort =
  | If
  | Loop
  | Bnd;

module Marker = {
  [@react.component]
  let make =
      (~kind=?, ~sort=?, ~text=?, ~loc=Guacamole.GCL.Loc.NoLoc, ~children) => {
    let kind =
      switch (kind) {
      | None => ""
      | Some(Guard) => " marker-guard"
      | Some(Assertion) => " marker-assertion"
      };
    let sort =
      switch (sort) {
      | None => ""
      | Some(If) => " marker-if"
      | Some(Loop) => " marker-loop"
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
let rec make = (~value: t) => {
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
    <Marker kind=Assertion sort=Loop text="loop invariant" loc>
      <Expr value=expr />
    </Marker>
  | GuardIf(expr, loc) =>
    <Marker kind=Guard sort=If loc text="guard"> <Expr value=expr /> </Marker>
  | GuardLoop(expr, loc) =>
    <Marker kind=Guard sort=Loop loc text="guard">
      <Expr value=expr />
    </Marker>
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