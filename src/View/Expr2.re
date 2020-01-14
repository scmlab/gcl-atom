open! Rebase;
open Decoder;
open Util;

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
  | Var(string, range)
  | Const(string, range)
  | Lit(Lit.t, range)
  | Op(Op.t, range)
  | App(t, t, range)
and subst = Js.Dict.t(t);

let rec decode: Json.Decode.decoder(t) =
  json =>
    json
    |> Json.Decode.(
         sum(
           fun
           | "Var" =>
             Contents(pair(string, range) |> map(((x, r)) => Var(x, r)))
           | "Const" =>
             Contents(pair(string, range) |> map(((x, r)) => Const(x, r)))
           | "Lit" =>
             Contents(
               pair(Lit.decode, range) |> map(((x, r)) => Lit(x, r)),
             )
           | "Op" =>
             Contents(pair(Op.decode, range) |> map(((x, r)) => Op(x, r)))
           | "App" =>
             Contents(
               tuple3(decode, decode, range)
               |> map(((x, y, r)) => App(x, y, r)),
             )
           | tag => raise(DecodeError("Unknown constructor: " ++ tag)),
         )
       )
and decodeSubst: Json.Decode.decoder(subst) =
  json => json |> Json.Decode.dict(decode);

module Precedence = {
  type stage =
    | Expect(t => stage)
    | Complete(string);

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
    | Mul => InfixL(7)
    | Div => InfixL(7)
    | Add => InfixL(8)
    | Sub => InfixL(8)
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
    | InfixL(m) =>
      Expect(
        p =>
          Expect(
            q =>
              Complete(
                parensIf(
                  n > m,
                  toString(m, p)
                  ++ " "
                  ++ Op.toString(op)
                  ++ " "
                  ++ toString(m + 1, q),
                ),
              ),
          ),
      )
    | InfixR(m) =>
      Expect(
        p =>
          Expect(
            q =>
              Complete(
                parensIf(
                  n > m,
                  toString(m + 1, p)
                  ++ " "
                  ++ Op.toString(op)
                  ++ " "
                  ++ toString(m, q),
                ),
              ),
          ),
      )
    | Infix(m) =>
      Expect(
        p =>
          Expect(
            q =>
              Complete(
                parensIf(
                  n > m,
                  toString(m + 1, p)
                  ++ " "
                  ++ Op.toString(op)
                  ++ " "
                  ++ toString(m + 1, q),
                ),
              ),
          ),
      )
    | Prefix(m) =>
      Expect(
        p =>
          Complete(
            parensIf(n > m, Op.toString(op) ++ " " ++ toString(m, p)),
          ),
      )
    | Postfix(m) =>
      Expect(
        p =>
          Complete(
            parensIf(n > m, toString(m, p) ++ " " ++ Op.toString(op)),
          ),
      )
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
  and toString = (n, p) =>
    switch (handleExpr(n, p)) {
    | Expect(_) => ""
    | Complete(s) => s
    };
};

let toString = Precedence.toString(0);
