// open Type.View;
open Rebase;
open React;
open Base;

module Origin = {
  type t =
    | AroundAbort(loc)
    | AroundSkip(loc)
    | AssertGuaranteed(loc)
    | AssertSufficient(loc)
    | Assignment(loc)
    | IfTotal(loc)
    | LoopBase(loc)
    | LoopTermBase(loc)
    | LoopInitialize(loc);

  open Decoder;
  open! Json.Decode;
  let decode: decoder(t) =
    sum(
      fun
      | "AroundAbort" => Contents(Loc.decode |> map(x => AroundAbort(x)))
      | "AroundSkip" => Contents(Loc.decode |> map(x => AroundSkip(x)))
      | "AssertGuaranteed" =>
        Contents(Loc.decode |> map(x => AssertGuaranteed(x)))
      | "AssertSufficient" =>
        Contents(Loc.decode |> map(x => AssertSufficient(x)))
      | "Assignment" => Contents(Loc.decode |> map(x => Assignment(x)))
      | "IfTotal" => Contents(Loc.decode |> map(x => IfTotal(x)))
      | "LoopBase" => Contents(Loc.decode |> map(x => LoopBase(x)))
      | "LoopTermBase" => Contents(Loc.decode |> map(x => LoopTermBase(x)))
      | "LoopInitialize" =>
        Contents(Loc.decode |> map(x => LoopInitialize(x)))
      | tag => raise(DecodeError("Unknown constructor: " ++ tag)),
    );

  let toString =
    fun
    | AroundAbort(_) => "AroundAbort"
    | AroundSkip(_) => "AroundSkip"
    | AssertGuaranteed(_) => "AssertGuaranteed"
    | AssertSufficient(_) => "AssertSufficient"
    | Assignment(_) => "Assignment"
    | IfTotal(_) => "IfTotal"
    | LoopBase(_) => "LoopBase"
    | LoopTermBase(_) => "LoopTermBase"
    | LoopInitialize(_) => "LoopInitialize";

  let locOf =
    fun
    | AroundAbort(l) => l
    | AroundSkip(l) => l
    | AssertGuaranteed(l) => l
    | AssertSufficient(l) => l
    | Assignment(l) => l
    | IfTotal(l) => l
    | LoopBase(l) => l
    | LoopTermBase(l) => l
    | LoopInitialize(l) => l;
};

module ProofObligation = {
  type t =
    | ProofObligation(int, Syntax.Pred.t, Syntax.Pred.t, Origin.t);
  // | IfTotal(Syntax.Expr.t, array(Syntax.Expr.t), Loc.t);

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
    // | IfTotal(p, qs, _) =>
    //   let qs' = qs |> Array.map(q => <Expr expr=q />);
    //
    //   <li className="gcl-body-item">
    //     <span className="gcl-proof-obligation-antecedent">
    //       <Expr expr=p />
    //     </span>
    //     <span className="gcl-proof-obligation-arrow">
    //       {string({j|⇒|j})}
    //     </span>
    //     <span className="gcl-proof-obligation-consequent">
    //       {string("DEBUG: if total")}
    //       // {string("either one of the following condition should hold")}
    //       {Util.React.sepBy(<br />, qs')}
    //     </span>
    //   </li>;
    // <Expr expr=q />
    };

  open! Json.Decode;
  let decode: decoder(t) =
    tuple4(int, Syntax.Pred.decode, Syntax.Pred.decode, Origin.decode)
    |> map(((i, p, q, o)) => ProofObligation(i, p, q, o));
  //
  // let decode: decoder(t) =
  //   sum(
  //     fun
  //     | "Obligation" =>
  //       Contents(
  //         tuple4(
  //           int,
  //           Syntax.Expr.decode,
  //           Syntax.Expr.decode,
  //           array(Origin.decode),
  //         )
  //         |> map(((i, p, q, o)) => ProofObligation(i, p, q, o)),
  //       )
  //     | "ObliIfTotal" =>
  //       Contents(
  //         tuple3(Syntax.Expr.decode, array(Syntax.Expr.decode), Loc.decode)
  //         |> map(((p, qs, l)) => IfTotal(p, qs, l)),
  //       )
  //     | tag => raise(DecodeError("Unknown constructor: " ++ tag)),
  //   );
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
