open Rebase;
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

module Expr = {
  type t =
    | Var(string)
    | Const(string)
    | Lit(Lit.t)
    | Ap(t, t)
    | Hole(int, array(subst))
  and subst = Js.Dict.t(t);

  let rec decode: Json.Decode.decoder(t) =
    json =>
      json
      |> Json.Decode.(
           sum(
             fun
             | "VarE" => Contents(string |> map(x => Var(x)))
             | "ConstE" => Contents(string |> map(x => Const(x)))
             | "LitE" => Contents(Lit.decode |> map(x => Lit(x)))
             | "ApE" =>
               Contents(pair(decode, decode) |> map(((x, y)) => Ap(x, y)))
             | "HoleE" =>
               Contents(
                 pair(int, array(decodeSubst))
                 |> map(((x, xs)) => Hole(x, xs)),
               )
             | tag => raise(DecodeError("Unknown constructor: " ++ tag)),
           )
         )
  and decodeSubst: Json.Decode.decoder(subst) =
    json => json |> Json.Decode.dict(decode);

  let intercalate = (toString, sep, xs) => {
    let rec intercalate' = xs =>
      switch (xs) {
      | [] => ""
      | [x] => toString(x)
      | [x, ...xs] => toString(x) ++ sep ++ intercalate'(xs)
      };
    intercalate'(List.fromArray(xs));
  };

  let rec toString =
    fun
    | Var(s) => s
    | Const(s) => s
    | Lit(lit) => Lit.toString(lit)
    | Ap(x, y) => toString(x) ++ " " ++ toString(y)
    | Hole(i, _substs) => "[" ++ string_of_int(i) ++ "]";
};

module BinRel = {
  type t =
    | Eq
    | LEq
    | GEq
    | LTh
    | GTh;

  let decode: Json.Decode.decoder(t) =
    Json.Decode.(
      string
      |> map(
           fun
           | "Eq" => Eq
           | "LEq" => LEq
           | "GEq" => GEq
           | "LTh" => LTh
           | "GTh" => GTh
           | tag => raise(DecodeError("Unknown constructor: " ++ tag)),
         )
    );

  let toString =
    fun
    | Eq => "="
    | LEq => "<="
    | GEq => ">="
    | LTh => "<"
    | GTh => ">";
};

// sorted by precedences in descending order
type t =
  | Implies(t, t) // right assoc
  | Disj(t, t) // left assoc
  | Conj(t, t) // left assoc
  | Term(BinRel.t, Expr.t, Expr.t)
  | Neg(t)
  | Lit(bool)
  | Hole(int);

let rec decode: Json.Decode.decoder(t) =
  json =>
    json
    |> Json.Decode.(
         sum(
           fun
           | "Term" =>
             Contents(
               tuple3(BinRel.decode, Expr.decode, Expr.decode)
               |> map(((rel, p, q)) => Term(rel, p, q)),
             )
           | "Implies" =>
             Contents(
               pair(decode, decode) |> map(((p, q)) => Implies(p, q)),
             )
           | "Conj" =>
             Contents(pair(decode, decode) |> map(((p, q)) => Conj(p, q)))
           | "Disj" =>
             Contents(pair(decode, decode) |> map(((p, q)) => Disj(p, q)))
           | "Neg" => Contents(decode |> map(p => Neg(p)))
           | "Lit" => Contents(bool |> map(p => Lit(p)))
           | "Hole" => Contents(int |> map(p => Hole(p)))
           | tag => raise(DecodeError("Unknown constructor: " ++ tag)),
         )
       );

let rec toStringPrec = n =>
  fun
  | Implies(p, q) =>
    parensIf(n > 0, toStringPrec(1, p) ++ {j| → |j} ++ toStringPrec(0, q))
  | Disj(p, q) =>
    parensIf(n > 1, toStringPrec(1, p) ++ {j| ⋁ |j} ++ toStringPrec(2, q))
  | Conj(p, q) =>
    parensIf(n > 2, toStringPrec(2, p) ++ {j| ⋀ |j} ++ toStringPrec(3, q))
  | Term(rel, p, q) =>
    parensIf(
      n > 3,
      Expr.toString(p)
      ++ " "
      ++ BinRel.toString(rel)
      ++ " "
      ++ Expr.toString(q),
    )
  | Neg(p) => parensIf(n > 4, {j|¬ |j} ++ toStringPrec(4, p))
  | Lit(true) => "true"
  | Lit(false) => "false"
  | Hole(int) => "?" ++ string_of_int(int);

let toString = toStringPrec(0);
