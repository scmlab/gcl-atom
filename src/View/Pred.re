open! Rebase;
open Decoder;

// adds parentheses when True
let parensIf = (p, s) =>
  if (p) {
    "(" ++ s ++ ")";
  } else {
    s;
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

let rec toString =
  fun
  | Var(s) => s
  | Const(s) => s
  | Lit(lit) => Lit.toString(lit)
  | Op(op) => Op.toString(op)
  | App(x, y) => toString(x) ++ " " ++ toString(y)
  | Hole(i, _substs) => "[" ++ string_of_int(i) ++ "]";

// module BinRel = {
//   type t =
//     | EQ
//     | LTE
//     | GTE
//     | LT
//     | GT;
//
//   let decode: Json.Decode.decoder(t) =
//     Json.Decode.(
//       string
//       |> map(
//            fun
//            | "EQ" => EQ
//            | "LTE" => LTE
//            | "GTE" => GTE
//            | "LT" => LT
//            | "GT" => GT
//            | tag => raise(DecodeError("Unknown constructor: " ++ tag)),
//          )
//     );
//
//   let toString =
//     fun
//     | EQ => "="
//     | LTE => "<="
//     | GTE => ">="
//     | LT => "<"
//     | GT => ">";
// };
//
// // sorted by precedences in descending order
// type t =
//   | Implies(t, t) // right assoc
//   | Disj(t, t) // left assoc
//   | Conj(t, t) // left assoc
//   | Term(BinRel.t, Expr.t, Expr.t)
//   | Neg(t)
//   | Lit(bool)
//   | Hole(int);
//
// let rec decode: Json.Decode.decoder(t) =
//   json =>
//     json
//     |> Json.Decode.(
//          sum(
//            fun
//            | "Term" =>
//              Contents(
//                tuple3(BinRel.decode, Expr.decode, Expr.decode)
//                |> map(((rel, p, q)) => Term(rel, p, q)),
//              )
//            | "Implies" =>
//              Contents(
//                pair(decode, decode) |> map(((p, q)) => Implies(p, q)),
//              )
//            | "Conj" =>
//              Contents(pair(decode, decode) |> map(((p, q)) => Conj(p, q)))
//            | "Disj" =>
//              Contents(pair(decode, decode) |> map(((p, q)) => Disj(p, q)))
//            | "Neg" => Contents(decode |> map(p => Neg(p)))
//            | "Lit" => Contents(bool |> map(p => Lit(p)))
//            | "Hole" => Contents(int |> map(p => Hole(p)))
//            | tag => raise(DecodeError("Unknown constructor: " ++ tag)),
//          )
//        );
//
// let rec toStringPrec = n =>
//   fun
//   | Implies(p, q) =>
//     parensIf(n > 0, toStringPrec(1, p) ++ {j| → |j} ++ toStringPrec(0, q))
//   | Disj(p, q) =>
//     parensIf(n > 1, toStringPrec(1, p) ++ {j| ⋁ |j} ++ toStringPrec(2, q))
//   | Conj(p, q) =>
//     parensIf(n > 2, toStringPrec(2, p) ++ {j| ⋀ |j} ++ toStringPrec(3, q))
//   | Term(rel, p, q) =>
//     parensIf(
//       n > 3,
//       Expr.toString(p)
//       ++ " "
//       ++ BinRel.toString(rel)
//       ++ " "
//       ++ Expr.toString(q),
//     )
//   | Neg(p) => parensIf(n > 4, {j|¬ |j} ++ toStringPrec(4, p))
//   | Lit(true) => "true"
//   | Lit(false) => "false"
//   | Hole(int) => "?" ++ string_of_int(int);
//
// let toString = toStringPrec(0);
