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
  type partialOrdering =
    | Equal
    | GreaterThan
    | LessThan
    | CantCompare;

  type stage =
    | Expect(t => stage)
    | Complete(string);

  open! Op;

  module Sort = {
    type t =
      | Predicate(int)
      | Number(int)
      | Others;
    let compareInt = (x, y) =>
      switch (compare(x, y)) {
      | (-1) => LessThan
      | 0 => Equal
      | _ => GreaterThan
      };
    let compare = (x, y) =>
      switch (x, y) {
      | (Predicate(m), Predicate(n)) => compareInt(m, n)
      | (Number(m), Number(n)) => compareInt(m, n)
      | _ => CantCompare
      };

    let succ =
      fun
      | Predicate(n) => Predicate(n + 1)
      | Number(n) => Number(n + 1)
      | Others => Others;
  };

  type fixity =
    | InfixL(Sort.t)
    | InfixR(Sort.t)
    | Infix(Sort.t)
    | Prefix(Sort.t)
    | Postfix(Sort.t);

  let classify =
    fun
    | Implies => InfixR(Predicate(1))
    | Disj => InfixL(Predicate(2))
    | Conj => InfixL(Predicate(3))
    | Neg => Prefix(Predicate(4))
    | EQ => Infix(Predicate(5))
    | NEQ => Infix(Predicate(5))
    | LTE => Infix(Predicate(5))
    | GTE => Infix(Predicate(5))
    | LT => Infix(Predicate(5))
    | GT => Infix(Predicate(5))
    | Mod => InfixL(Number(1))
    | Mul => InfixL(Number(2))
    | Div => InfixL(Number(2))
    | Add => InfixL(Number(3))
    | Sub => InfixL(Number(3));

  // adds parentheses when True
  let parensIf = (p, s) =>
    if (p) {
      "(" ++ s ++ ")";
    } else {
      s;
    };

  let greaterThan = (x, y) => Sort.compare(x, y) == GreaterThan;

  let rec handleOperator = (n, op) =>
    switch (classify(op)) {
    | InfixL(m) =>
      Expect(
        p =>
          Expect(
            q =>
              Complete(
                parensIf(
                  greaterThan(n, m),
                  toString(m, p)
                  ++ " "
                  ++ Op.toString(op)
                  ++ " "
                  ++ toString(Sort.succ(m), q),
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
                  greaterThan(n, m),
                  toString(Sort.succ(m), p)
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
                  greaterThan(n, m),
                  toString(Sort.succ(m), p)
                  ++ " "
                  ++ Op.toString(op)
                  ++ " "
                  ++ toString(Sort.succ(m), q),
                ),
              ),
          ),
      )
    | Prefix(m) =>
      Expect(
        p =>
          Complete(
            parensIf(
              greaterThan(n, m),
              Op.toString(op) ++ " " ++ toString(m, p),
            ),
          ),
      )
    | Postfix(m) =>
      Expect(
        p =>
          Complete(
            parensIf(
              greaterThan(n, m),
              toString(m, p) ++ " " ++ Op.toString(op),
            ),
          ),
      )
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

let toString = Precedence.toString(Others);
