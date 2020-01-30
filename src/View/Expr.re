// open Rebase;
open React;
open Base;

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
        <div className onMouseOver onMouseLeave onClick>
          {string("(...)")}
        </div>;
      } else {
        <>
          <div className onMouseOver onMouseLeave onClick>
            {string("(")}
          </div>
          children
          <div className onMouseOver onMouseLeave onClick>
            {string(")")}
          </div>
        </>;
      };
    } else {
      children;
    };
  };
};

module Space = {
  [@react.component]
  let make = () => <div> {string(" ")} </div>;
};

module Operator = {
  open Syntax.Op;

  [@react.component]
  let make = (~value: Syntax.Op.t, ~loc: loc) =>
    <Link loc> {string(toString(value))} </Link>;
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
      | Var(s, loc) => Complete(<Link loc> {string(s)} </Link>)
      | Const(s, loc) => Complete(<Link loc> {string(s)} </Link>)
      | Lit(lit, loc) =>
        Complete(<Link loc> {string(Lit.toString(lit))} </Link>)
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
            | _ => Complete(<> s <Space /> t </>)
            }
          }
        }
      | Hole(loc) => Complete(<Link loc> {string("[?]")} </Link>)
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
let make = (~value: Syntax.Expr.t) =>
  <div className="expr"> <Prec prec=0 value /> </div>;
