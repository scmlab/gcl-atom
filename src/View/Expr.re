open! Rebase;
open Decoder;

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
    | Mul => "*"
    | Div => "/"
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

type t =
  | Var(string)
  | Const(string)
  | Lit(Lit.t)
  | Op(Op.t)
  | App(t, t)
  | Hole(int, array(subst))
and subst = Js.Dict.t(t);

let rec decode: Json.Decode.decoder(t) =
  json =>
    json
    |> Json.Decode.(
         sum(
           fun
           | "Var" => Contents(string |> map(x => Var(x)))
           | "Const" => Contents(string |> map(x => Const(x)))
           | "Lit" => Contents(Lit.decode |> map(x => Lit(x)))
           | "Op" => Contents(Op.decode |> map(x => Op(x)))
           | "App" =>
             Contents(pair(decode, decode) |> map(((x, y)) => App(x, y)))
           | "Hole" =>
             Contents(
               pair(int, array(decodeSubst))
               |> map(((x, xs)) => Hole(x, xs)),
             )
           | tag => raise(DecodeError("Unknown constructor: " ++ tag)),
         )
       )
and decodeSubst: Json.Decode.decoder(subst) =
  json => json |> Json.Decode.dict(decode);

module Precedence = {
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
      Complete(parensIf(n > m, Op.toString(op) ++ " " ++ toString(m, p)));
    | Postfix(m) =>
      let%VarArg p = var;
      Complete(parensIf(n > m, toString(m, p) ++ " " ++ Op.toString(op)));
    }
  and handleExpr = n =>
    fun
    | Var(s) => Complete(s)
    | Const(s) => Complete(s)
    | Lit(lit) => Complete(Lit.toString(lit))
    | Op(op) => handleOperator(n, op)
    | App(p, q) =>
      switch (handleExpr(n, p)) {
      | Expect(f) => f(q)
      | Complete(s) =>
        switch (handleExpr(n, q)) {
        | Expect(g) => Expect(g)
        | Complete(t) =>
          switch (q) {
          | App(_, _) => Complete(s ++ " " ++ parensIf(true, t))
          | _ => Complete(s ++ " " ++ t)
          }
        }
      }
    | Hole(i, _substs) => Complete("[" ++ string_of_int(i) ++ "]")
  and toString = (n, p) =>
    switch (handleExpr(n, p)) {
    | Expect(_) => ""
    | Complete(s) => s
    };
};

let toString = Precedence.toString(0);
