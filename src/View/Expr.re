// open Rebase;
open React;

open Syntax;

[@react.component]
let make = _ => <span> {string("hi")} </span>;
// fun
// | Var(s, _) => <span> {string(s)} </span>
// | Const(s, _) => <span> {string(s)} </span>
// | Lit(lit, _) => <span> {string(Lit.toString(lit))} </span>
// | Op(op, _) =>
//
// handleOperator(n, op)
// | App(p, q, _) =>
//   switch (handleExpr(n, p)) {
//   | Expect(f) => f(q)
//   | Complete(s) =>
//     switch (handleExpr(n, q)) {
//     | Expect(g) => Expect(g)
//     | Complete(t) =>
//       switch (q) {
//       | App(_, _, _) => Complete(s ++ " " ++ parensIf(true, t))
//       | _ => Complete(s ++ " " ++ t)
//       }
//     }
//   }
// | Hole(_) => Complete("[?]");
// (~body: t) => {
// switch (body) {
// | Nothing => <> </>
// | ProofObligations([||]) => <> </>
// | ProofObligations(ps) =>
//   let list =
//     ps
//     |> Array.map(payload => <ProofObligation payload />)
//     |> Util.React.manyIn(
//          "ul",
//          ~props=
//            ReactDOMRe.domProps(~className="gcl-proof-obligation-list", ()),
//        );
//   <div className="gcl-body"> list </div>;
//
// | Plain(s) =>
//   let paragraphs =
//     s
//     |> Js.String.split("\n")
//     |> Array.filter(x => !String.isEmpty(x))
//     |> Array.map(s => <p> {string(s)} </p>)
//     |> Util.React.manyIn(
//          "div",
//          ~props=
//            ReactDOMRe.domProps(
//              ~className="gcl-plain-text gcl-body-item",
//              (),
//            ),
//        );
//   <div className="gcl-body"> paragraphs </div>;
// };
// };
