open Rebase;
open Rebase.Fn;

open Base;

// for pretty printing
module VarArg = {
  type t('a, 'b) =
    | Expect('a => t('a, 'b))
    | Complete('b);

  let return = x => Complete(x);
  let rec flatMap = (x, f) =>
    switch (x) {
    | Expect(g) => Expect(x => flatMap(g(x), f))
    | Complete(x) => f(x)
    };
  let let_ = flatMap;

  let var = Expect(x => Complete(x));
};

module Lit = {
  type t =
    | Num(int)
    | Bool(bool);

  let toString =
    fun
    | Num(i) => string_of_int(i)
    | Bool(true) => "True"
    | Bool(false) => "False";

  open Util.Decode;
  open Json.Decode;
  let decode: decoder(t) =
    json =>
      json
      |> sum(
           fun
           | "Num" => Contents(int |> map(x => Num(x)))
           | "Bol" => Contents(bool |> map(x => Bool(x)))
           | tag => raise(DecodeError("Unknown constructor: " ++ tag)),
         );
};

module Op = {
  type t =
    | EQ
    | NEQ
    | LTE
    | GTE
    | LT
    | GT
    | Implies
    | Conj
    | Disj
    | Neg
    | Add
    | Sub
    | Mul
    | Div
    | Mod;

  let toString =
    fun
    | EQ => "="
    | NEQ => {j|≠|j}
    | LTE => {j|≤|j}
    | GTE => {j|≥|j}
    | LT => "<"
    | GT => ">"
    | Implies => {j|→|j}
    | Disj => {j|∨|j}
    | Conj => {j|∧|j}
    | Neg => {j|¬|j}
    | Add => "+"
    | Sub => "-"
    | Mul => {j|×|j}
    | Div => {j|÷|j}
    | Mod => "%";

  open Json.Decode;
  let decode: decoder(t) =
    string
    |> map(
         fun
         | "EQ" => EQ
         | "NEQ" => NEQ
         | "LTE" => LTE
         | "GTE" => GTE
         | "LT" => LT
         | "GT" => GT
         | "Implies" => Implies
         | "Conj" => Conj
         | "Disj" => Disj
         | "Neg" => Neg
         | "Add" => Add
         | "Sub" => Sub
         | "Mul" => Mul
         | "Div" => Div
         | "Mod" => Mod
         | tag => raise(DecodeError("Unknown constructor: " ++ tag)),
       );
};

module Upper = {
  type t =
    | Upper(string, loc);
  let toString =
    fun
    | Upper(x, _) => x;
  open Json.Decode;
  let decode: decoder(t) =
    pair(string, Loc.decode) |> map(((x, r)) => Upper(x, r));
};

module Lower = {
  type t =
    | Lower(string, loc);
  let toString =
    fun
    | Lower(x, _) => x;
  open Json.Decode;
  let decode: decoder(t) =
    pair(string, Loc.decode) |> map(((x, r)) => Lower(x, r));
};

module Expr = {
  type t =
    | Var(string, loc)
    | Const(string, loc)
    | Lit(Lit.t, loc)
    | Op(Op.t, loc)
    | App(t, t, loc)
    | Hole(loc)
  and subst = Js.Dict.t(t);

  let locOf =
    fun
    | Var(_, loc) => loc
    | Const(_, loc) => loc
    | Lit(_, loc) => loc
    | Op(_, loc) => loc
    | App(_, _, loc) => loc
    | Hole(loc) => loc;

  let negate = x => App(Op(Op.Neg, NoLoc), x, NoLoc);
  let disj = (x, y) => App(App(Op(Op.Disj, NoLoc), x, NoLoc), y, NoLoc);
  let conj = (x, y) => App(App(Op(Op.Conj, NoLoc), x, NoLoc), y, NoLoc);
  let rec disjunct' =
    fun
    | [] => Lit(Bool(true), NoLoc)
    | [x] => x
    | [x, ...xs] => disj(x, disjunct'(xs));
  let rec conjunct' =
    fun
    | [] => Lit(Bool(false), NoLoc)
    | [x] => x
    | [x, ...xs] => disj(x, conjunct'(xs));
  let disjunct = List.fromArray >> disjunct';
  let conjunct = List.fromArray >> conjunct';

  open Util.Decode;
  open Json.Decode;
  let rec decode: decoder(t) =
    json =>
      json
      |> sum(
           fun
           | "Var" =>
             Contents(
               pair(Lower.decode, Loc.decode)
               |> map(((x, r)) => Var(Lower.toString(x), r)),
             )
           | "Const" =>
             Contents(
               pair(Upper.decode, Loc.decode)
               |> map(((x, r)) => Const(Upper.toString(x), r)),
             )
           | "Lit" =>
             Contents(
               pair(Lit.decode, Loc.decode) |> map(((x, r)) => Lit(x, r)),
             )
           | "Op" =>
             Contents(
               pair(Op.decode, Loc.decode) |> map(((x, r)) => Op(x, r)),
             )
           | "App" =>
             Contents(
               tuple3(decode, decode, Loc.decode)
               |> map(((x, y, r)) => App(x, y, r)),
             )
           | "Hole" => Contents(Loc.decode |> map(r => Hole(r)))
           | tag => raise(DecodeError("Unknown constructor: " ++ tag)),
         )
  and decodeSubst: decoder(subst) = json => json |> dict(decode);

  module Precedence = {
    open VarArg;

    open! Op;

    type fixity =
      | InfixL(int)
      | InfixR(int)
      | Infix(int)
      | Prefix(int)
      | Postfix(int);

    let classify =
      fun
      | Implies => InfixR(1)
      | Disj => InfixL(2)
      | Conj => InfixL(3)
      | Neg => Prefix(4)
      | EQ => Infix(5)
      | NEQ => Infix(6)
      | LTE => Infix(6)
      | GTE => Infix(6)
      | LT => Infix(6)
      | GT => Infix(6)
      | Add => InfixL(7)
      | Sub => InfixL(7)
      | Mul => InfixL(8)
      | Div => InfixL(8)
      | Mod => InfixL(9);

    // adds parentheses when True
    let parensIf = (p, s) =>
      if (p) {
        "(" ++ s ++ ")";
      } else {
        s;
      };

    let rec handleOperator = (n, op) =>
      switch (classify(op)) {
      | Infix(m) =>
        let%VarArg p = var;
        let%VarArg q = var;
        Complete(
          parensIf(
            n > m,
            toString(m + 1, p)
            ++ " "
            ++ Op.toString(op)
            ++ " "
            ++ toString(m + 1, q),
          ),
        );
      | InfixL(m) =>
        let%VarArg p = var;
        let%VarArg q = var;

        Complete(
          parensIf(
            n > m,
            toString(m, p)
            ++ " "
            ++ Op.toString(op)
            ++ " "
            ++ toString(m + 1, q),
          ),
        );
      | InfixR(m) =>
        let%VarArg p = var;
        let%VarArg q = var;
        Complete(
          parensIf(
            n > m,
            toString(m + 1, p)
            ++ " "
            ++ Op.toString(op)
            ++ " "
            ++ toString(m, q),
          ),
        );
      | Prefix(m) =>
        let%VarArg p = var;
        Complete(
          parensIf(n > m, Op.toString(op) ++ " " ++ toString(m, p)),
        );
      | Postfix(m) =>
        let%VarArg p = var;
        Complete(
          parensIf(n > m, toString(m, p) ++ " " ++ Op.toString(op)),
        );
      }
    and handleExpr = n =>
      fun
      | Var(s, _) => Complete(s)
      | Const(s, _) => Complete(s)
      | Lit(lit, _) => Complete(Lit.toString(lit))
      | Op(op, _) => handleOperator(n, op)
      | App(p, q, _) =>
        switch (handleExpr(n, p)) {
        | Expect(f) => f(q)
        | Complete(s) =>
          switch (handleExpr(n, q)) {
          | Expect(g) => Expect(g)
          | Complete(t) =>
            switch (q) {
            | App(_, _, _) => Complete(s ++ " " ++ parensIf(true, t))
            | _ => Complete(s ++ " " ++ t)
            }
          }
        }
      | Hole(_) => Complete("[?]")
    // | Hole(_) => Complete("[" ++ string_of_int(i) ++ "]")
    and toString = (n, p) =>
      switch (handleExpr(n, p)) {
      | Expect(_) => ""
      | Complete(s) => s
      };
  };

  let toString = Precedence.toString(0);
};

module Pred = {
  type t =
    | Constant(Expr.t)
    | Bound(Expr.t, Loc.t)
    | Assertion(Expr.t, Loc.t)
    | LoopInvariant(Expr.t, Expr.t, Loc.t)
    | GuardIf(Expr.t, Loc.t)
    | GuardLoop(Expr.t, Loc.t)
    | Conjunct(array(t))
    | Disjunct(array(t))
    | Negate(t);

  open Util.Decode;
  open Json.Decode;
  let rec decode: decoder(t) =
    json =>
      json
      |> sum(
           fun
           | "Constant" => Contents(Expr.decode |> map(x => Constant(x)))
           | "Bound" =>
             Contents(
               pair(Expr.decode, Loc.decode)
               |> map(((e, l)) => Bound(e, l)),
             )
           | "Assertion" =>
             Contents(
               pair(Expr.decode, Loc.decode)
               |> map(((e, l)) => Assertion(e, l)),
             )
           | "LoopInvariant" =>
             Contents(
               tuple3(Expr.decode, Expr.decode, Loc.decode)
               |> map(((e, bnd, l)) => LoopInvariant(e, bnd, l)),
             )
           | "GuardIf" =>
             Contents(
               pair(Expr.decode, Loc.decode)
               |> map(((e, l)) => GuardIf(e, l)),
             )
           | "GuardLoop" =>
             Contents(
               pair(Expr.decode, Loc.decode)
               |> map(((e, l)) => GuardLoop(e, l)),
             )
           | "Conjunct" =>
             Contents(array(decode) |> map(xs => Conjunct(xs)))
           | "Disjunct" =>
             Contents(array(decode) |> map(xs => Disjunct(xs)))
           | "Negate" => Contents(decode |> map(x => Negate(x)))
           | tag => raise(DecodeError("Unknown constructor: " ++ tag)),
         );

  let rec toExpr =
    fun
    | Constant(e) => e
    | Bound(e, _) => e
    | Assertion(e, _) => e
    | LoopInvariant(e, _, _) => e
    | GuardIf(e, _) => e
    | GuardLoop(e, _) => e
    | Conjunct(xs) => xs |> Array.map(toExpr) |> Expr.disjunct
    | Disjunct(xs) => xs |> Array.map(toExpr) |> Expr.conjunct
    | Negate(x) => x |> toExpr |> Expr.negate;

  let toString = toExpr >> Expr.toString;
};

module Type = {
  module Base = {
    type t =
      | Int
      | Bool;

    let toString =
      fun
      | Int => "Int"
      | Bool => "Bool";

    open Json.Decode;
    let decode: decoder(t) =
      string
      |> map(
           fun
           | "TInt" => Int
           | "TBool" => Bool
           | tag => raise(DecodeError("Unknown constructor: " ++ tag)),
         );
  };

  type t =
    | Base(Base.t)
    | Array(t)
    | Func(t, t)
    | Var(int);

  let rec toString =
    fun
    | Base(b) => Base.toString(b)
    | Array(t) => "Array " ++ toString(t)
    | Func(s, t) => toString(s) ++ " -> " ++ toString(t)
    | Var(i) => "Var " ++ string_of_int(i);

  open Util.Decode;
  open Json.Decode;
  let rec decode: decoder(t) =
    json =>
      json
      |> sum(
           fun
           | "TBase" => Contents(Base.decode |> map(x => Base(x)))
           | "TArray" => Contents(decode |> map(x => Array(x)))
           | "TFun" =>
             Contents(pair(decode, decode) |> map(((x, y)) => Func(x, y)))
           | "TVar" => Contents(int |> map(x => Var(x)))
           | tag => raise(DecodeError("Unknown constructor: " ++ tag)),
         );
};
