// open Type.View;
open Rebase;
open React;
open Base;

module Origin = {
  type t =
    | AtAbort(loc)
    | AtSkip(loc)
    | AtSpec(loc)
    | AtAssignment(loc)
    | AtAssertion(loc)
    | AtLoopInvariant(loc)
    | AtIf(loc)
    | AtLoop(loc)
    | AtTermination(loc)
    | AtBoundDecrement(loc);

  open Decoder;
  open! Json.Decode;
  let decode: decoder(t) =
    sum(
      fun
      | "AtAbort" => Contents(Loc.decode |> map(x => AtAbort(x)))
      | "AtSkip" => Contents(Loc.decode |> map(x => AtSkip(x)))
      | "AtSpec" => Contents(Loc.decode |> map(x => AtSpec(x)))
      | "AtAssignment" => Contents(Loc.decode |> map(x => AtAssignment(x)))
      | "AtAssertion" => Contents(Loc.decode |> map(x => AtAssertion(x)))
      | "AtLoopInvariant" =>
        Contents(Loc.decode |> map(x => AtLoopInvariant(x)))
      | "AtIf" => Contents(Loc.decode |> map(x => AtIf(x)))
      | "AtLoop" => Contents(Loc.decode |> map(x => AtLoop(x)))
      | "AtTermination" => Contents(Loc.decode |> map(x => AtTermination(x)))
      | "AtBoundDecrement" =>
        Contents(Loc.decode |> map(x => AtBoundDecrement(x)))
      | tag => raise(DecodeError("Unknown constructor: " ++ tag)),
    );

  let toString =
    fun
    | AtAbort(_) => "Abort"
    | AtSkip(_) => "Skip"
    | AtSpec(_) => "Spec"
    | AtAssignment(_) => "Assignment"
    | AtAssertion(_) => "Assertion"
    | AtLoopInvariant(_) => "Loop Invariant"
    | AtIf(_) => "Conditional"
    | AtLoop(_) => "Loop"
    | AtTermination(_) => "Termination"
    | AtBoundDecrement(_) => "Bound Decrement";

  let locOf =
    fun
    | AtAbort(loc) => loc
    | AtSkip(loc) => loc
    | AtSpec(loc) => loc
    | AtAssignment(loc) => loc
    | AtAssertion(loc) => loc
    | AtLoopInvariant(loc) => loc
    | AtIf(loc) => loc
    | AtLoop(loc) => loc
    | AtTermination(loc) => loc
    | AtBoundDecrement(loc) => loc;
};

module ProofObligation = {
  type t =
    | ProofObligation(int, Syntax.Pred.t, Syntax.Pred.t, Origin.t);

  [@react.component]
  let make = (~payload: t) =>
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

  open! Json.Decode;
  let decode: decoder(t) =
    tuple4(int, Syntax.Pred.decode, Syntax.Pred.decode, Origin.decode)
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
