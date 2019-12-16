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
    | LTE
    | GTE
    | LT
    | GT
    | Implies
    | Conj
    | Disj
    | Neg
    | Plus
    | Minus
    | Mul
    | Div;

  let toString =
    fun
    | EQ => "="
    | LTE => "<="
    | GTE => ">="
    | LT => "<"
    | GT => ">"
    | Implies => {j|→|j}
    | Disj => {j|⋁|j}
    | Conj => {j|⋀|j}
    | Neg => {j|¬|j}
    | Plus => "+"
    | Minus => "-"
    | Mul => "*"
    | Div => "/";

  open Json.Decode;
  let decode: decoder(t) =
    string
    |> map(
         fun
         | "EQ" => EQ
         | "LTE" => LTE
         | "GTE" => GTE
         | "LT" => LT
         | "GT" => GT
         | "Implies" => Implies
         | "Conj" => Conj
         | "Disj" => Disj
         | "Neg" => Neg
         | "Plus" => Plus
         | "Minus" => Minus
         | "Mul" => Mul
         | "Div" => Div
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

// let intercalate = (toString, sep, xs) => {
//   let rec intercalate' = xs =>
//     switch (xs) {
//     | [] => ""
//     | [x] => toString(x)
//     | [x, ...xs] => toString(x) ++ sep ++ intercalate'(xs)
//     };
//   intercalate'(List.fromArray(xs));
// };

// let rec toString =
//   fun
//   | Var(s) => s
//   | Const(s) => s
//   | Lit(lit) => Lit.toString(lit)
//   | Op(op) => Op.toString(op)
//   | App(x, y) => toString(x) ++ " " ++ toString(y)
//   | Hole(i, _substs) => "[" ++ string_of_int(i) ++ "]";

module Precedence = {
  open Op;
  type partialOrdering =
    | Equal
    | GreaterThan
    | LessThan
    | CantCompare;

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

  type t =
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
    | LTE => Infix(Predicate(5))
    | GTE => Infix(Predicate(5))
    | LT => Infix(Predicate(5))
    | GT => Infix(Predicate(5))
    | Mul => InfixL(Number(1))
    | Div => InfixL(Number(1))
    | Plus => InfixL(Number(2))
    | Minus => InfixL(Number(2));

  // adds parentheses when True
  let parensIf = (p, s) =>
    if (p) {
      "(" ++ s ++ ")";
    } else {
      s;
    };

  let greaterThan = (x, y) => Sort.compare(x, y) == GreaterThan;

  let rec binary = (n, op, p, q) =>
    switch (classify(op)) {
    | InfixL(m) =>
      parensIf(
        greaterThan(n, m),
        toString(m, p)
        ++ " "
        ++ Op.toString(op)
        ++ " "
        ++ toString(Sort.succ(m), q),
      )
    | InfixR(m) =>
      parensIf(
        greaterThan(n, m),
        toString(Sort.succ(m), p)
        ++ " "
        ++ Op.toString(op)
        ++ " "
        ++ toString(m, q),
      )
    | Infix(m) =>
      parensIf(
        greaterThan(n, m),
        toString(Sort.succ(m), p)
        ++ " "
        ++ Op.toString(op)
        ++ " "
        ++ toString(Sort.succ(m), q),
      )
    | Prefix(_) =>
      Op.toString(op) ++ " " ++ toString(n, p) ++ toString(n, q)
    | Postfix(_) =>
      toString(n, p) ++ toString(n, q) ++ " " ++ Op.toString(op)
    }

  and toString = n =>
    fun
    | Var(s) => s
    | Const(s) => s
    | Lit(lit) => Lit.toString(lit)
    | Op(op) => Op.toString(op)
    // arity 2
    | App(App(Op(op), p), q) => binary(n, op, p, q)
    // arity 1
    | App(Op(Neg), y) =>
      parensIf(
        greaterThan(n, Predicate(4)),
        {j|¬ |j} ++ toString(Predicate(4), y),
      )
    | App(x, y) => toString(Others, x) ++ " " ++ toString(Others, y)
    | Hole(i, _substs) => "[" ++ string_of_int(i) ++ "]";
};

// | Disj(p, q) =>
//   parensIf(n > 1, toStringPrec(1, p) ++ {j| ⋁ |j} ++ toStringPrec(2, q))
// | Conj(p, q) =>
//   parensIf(n > 2, toStringPrec(2, p) ++ {j| ⋀ |j} ++ toStringPrec(3, q))
// | Term(rel, p, q) =>
//   parensIf(
//     n > 3,
//     Expr.toString(p)
//     ++ " "
//     ++ BinRel.toString(rel)
//     ++ " "
//     ++ Expr.toString(q),
//   )
// | Neg(p) => parensIf(n > 4, {j|¬ |j} ++ toStringPrec(4, p))
// | Lit(true) => "true"
// | Lit(false) => "false"
// | Hole(int) => "?" ++ string_of_int(int);

let toString = Precedence.toString(Others);
