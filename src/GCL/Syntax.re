open! Rebase;
open Decoder;
open! Util;

module Pos = {
  type t =
    | Pos(string, int, int);
  open Json.Decode;
  let decode: decoder(t) =
    json =>
      Pos(
        field("filepath", string, json),
        field("line", int, json),
        field("column", int, json),
      );

  let toPoint =
    fun
    | Pos(_, line, column) => Atom.Point.make(line - 1, column - 1);

  let toString =
    fun
    | Pos(_, line, column) =>
      string_of_int(line) ++ ":" ++ string_of_int(column);

  let translate = by =>
    fun
    | Pos(path, line, column) => {
        let Pos(_, y, x) = by;
        Pos(path, line + y, column + x);
      };

  let translateBy = (y, x) =>
    fun
    | Pos(path, line, column) => Pos(path, line + y, column + x);
};

module Loc = {
  type t =
    | NoLoc
    | Loc(Pos.t, Pos.t);
  open Json.Decode;
  let decode: decoder(t) =
    sum(
      fun
      | "Loc" =>
        Contents(
          json =>
            Loc(
              field("start", Pos.decode, json),
              field("end", Pos.decode, json),
            ),
        )
      | "NoLoc" => TagOnly(_ => NoLoc)
      | tag => raise(DecodeError("Unknown constructor: " ++ tag)),
    );

  let toRange =
    fun
    | NoLoc => Atom.Range.make(Atom.Point.make(0, 0), Atom.Point.make(0, 0))
    | Loc(x, Pos(_, line, column)) =>
      Atom.Range.make(Pos.toPoint(x), Atom.Point.make(line - 1, column));

  let toString =
    fun
    | NoLoc => "NoLoc"

    | Loc(x, y) => Pos.toString(x) ++ "-" ++ Pos.toString(y);

  let translate = by =>
    fun
    | NoLoc => by

    | Loc(x, y) =>
      switch (by) {
      | NoLoc => Loc(x, y)
      | Loc(w, v) => Loc(Pos.translate(x, w), Pos.translate(y, v))
      };

  let translateBy = (startY, startX, endY, endX) =>
    fun
    | NoLoc => Loc(Pos("", startY, startX), Pos("", endY, endX))
    | Loc(x, y) =>
      Loc(
        Pos.translateBy(startY, startX, x),
        Pos.translateBy(endY, endX, y),
      );
};

type pos = Pos.t;
type loc = Loc.t;

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

  let decode: Json.Decode.decoder(t) =
    json =>
      json
      |> Json.Decode.(
           sum(
             fun
             | "Num" => Contents(int |> map(x => Num(x)))
             | "Bol" => Contents(bool |> map(x => Bool(x)))
             | tag => raise(DecodeError("Unknown constructor: " ++ tag)),
           )
         );

  let toString =
    fun
    | Num(i) => string_of_int(i)
    | Bool(b) => string_of_bool(b);
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
  open Json.Decode;
  let decode: decoder(t) =
    pair(string, Loc.decode) |> map(((x, r)) => Upper(x, r));
  let toString =
    fun
    | Upper(x, _) => x;
};

module Lower = {
  type t =
    | Lower(string, loc);
  open Json.Decode;
  let decode: decoder(t) =
    pair(string, Loc.decode) |> map(((x, r)) => Lower(x, r));
  let toString =
    fun
    | Lower(x, _) => x;
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

  // let disjunct = Array.reduce(3, 4);

  let rec decode: Json.Decode.decoder(t) =
    json =>
      json
      |> Json.Decode.(
           sum(
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
         )
  and decodeSubst: Json.Decode.decoder(subst) =
    json => json |> Json.Decode.dict(decode);

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
