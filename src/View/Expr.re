// open Rebase;
open React;

// open Syntax;
// open Syntax.VarArg;

//
module Paren = {
  [@react.component]
  let make = (~activate, ~children) => {
    let (hovered, setHover) = Hook.useState(false);
    let (collapsed, setCollapse) = Hook.useState(false);
    let onMouseOver = _ => setHover(true);
    let onMouseLeave = _ => setHover(false);
    let onClick = _ => setCollapse(collapsed ? false : true);
    let className =
      "expr-paren"
      ++ (hovered ? " hovered" : "")
      ++ (collapsed ? " collapsed" : "");

    if (activate) {
      if (collapsed) {
        <span className onMouseOver onMouseLeave onClick>
          {string("(...)")}
        </span>;
      } else {
        <>
          <span className onMouseOver onMouseLeave onClick>
            {string("(")}
          </span>
          children
          <span className onMouseOver onMouseLeave onClick>
            {string(")")}
          </span>
        </>;
      };
    } else {
      children;
    };
  };
};

module Space = {
  [@react.component]
  let make = () => <span> {string(" ")} </span>;
};

module Operator = {
  open Syntax.Op;

  [@react.component]
  let make = (~value: Syntax.Op.t, ~loc: Atom.Range.t) =>
    <span> {string(toString(value))} </span>;
};

module Prec = {
  open Syntax;
  open Syntax.VarArg;
  // module VarArg = Syntax.VarArg;

  let rec handleOperator = (n, op, loc) => {
    module Self = {
      let make = make;
      let makeProps = makeProps;
    };
    switch (Syntax.Expr.Precedence.classify(op)) {
    | Infix(m) =>
      let%VarArg p = var;
      let%VarArg q = var;
      Complete(
        <Paren activate={n > m}>
          <Self prec={m + 1} value=p />
          <Space />
          <Operator value=op loc />
          <Space />
          <Self prec={m + 1} value=q />
        </Paren>,
      );
    | InfixL(m) =>
      let%VarArg p = var;
      let%VarArg q = var;
      Complete(
        <Paren activate={n > m}>
          <Self prec=m value=p />
          <Space />
          <Operator value=op loc />
          <Space />
          <Self prec={m + 1} value=q />
        </Paren>,
      );
    | InfixR(m) =>
      let%VarArg p = var;
      let%VarArg q = var;
      Complete(
        <Paren activate={n > m}>
          <Self prec={m + 1} value=p />
          <Space />
          <Operator value=op loc />
          <Space />
          <Self prec=m value=q />
        </Paren>,
      );
    | Prefix(m) =>
      let%VarArg p = var;
      Complete(
        <Paren activate={n > m}>
          <Operator value=op loc />
          <Space />
          <Self prec=m value=p />
        </Paren>,
      );
    | Postfix(m) =>
      let%VarArg p = var;
      Complete(
        <Paren activate={n > m}>
          <Self prec=m value=p />
          <Space />
          <Operator value=op loc />
        </Paren>,
      );
    };
  }
  and handleExpr = n => {
    Syntax.Expr.(
      fun
      | Var(s, _) => Complete(<span> {string(s)} </span>)
      | Const(s, _) => Complete(<span> {string(s)} </span>)
      | Lit(lit, _) => Complete(<span> {string(Lit.toString(lit))} </span>)
      | Op(op, loc) => handleOperator(n, op, loc)
      | App(p, q, _) =>
        switch (handleExpr(n, p)) {
        | Expect(f) => f(q)
        | Complete(s) =>
          switch (handleExpr(n, q)) {
          | Expect(g) => Expect(g)
          | Complete(t) =>
            switch (q) {
            | App(_, _, _) =>
              Complete(<> s <Paren activate=true> t </Paren> </>)
            | _ => Complete(<> s t </>)
            }
          }
        }
      | Hole(_) => Complete(<span> {string("[?]")} </span>)
    );
  }
  [@react.component]
  and make = (~prec, ~value) =>
    switch (handleExpr(prec, value)) {
    | Expect(_) => <> </>
    | Complete(s) => s
    };
};

[@react.component]
let make = (~expr: Syntax.Expr.t) => <Prec prec=0 value=expr />;
