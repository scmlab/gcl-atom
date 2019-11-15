open Rebase;
open Decoder;

module Lit = {
  type t =
    | Num(int)
    | Bool(bool);

  let decode: Json.Decode.decoder(t) =
    json =>
      json
      |> Json.Decode.(
           fields(
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
    | Op(t, array(t))
    | Hole(int, array(subst))
  and subst = Js.Dict.t(t);

  let rec decode: Json.Decode.decoder(t) =
    json =>
      json
      |> Json.Decode.(
           fields(
             fun
             | "VarE" => Contents(string |> map(x => Var(x)))
             | "ConstE" => Contents(string |> map(x => Const(x)))
             | "LitE" => Contents(Lit.decode |> map(x => Lit(x)))
             | "OpE" =>
               Contents(
                 pair(decode, array(decode))
                 |> map(((x, xs)) => Op(x, xs)),
               )
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
    | Op(x, xs) =>
      toString(x) ++ "(" ++ intercalate(toString, ", ", xs) ++ ")"
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

type t =
  | Term(BinRel.t, Expr.t, Expr.t)
  | Implies(t, t)
  | Conj(t, t)
  | Disj(t, t)
  | Neg(t)
  | Lit(bool)
  | Hole(int);

let rec decode: Json.Decode.decoder(t) =
  json =>
    json
    |> Json.Decode.(
         fields(
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

let rec toString =
  fun
  | Term(rel, p, q) =>
    Expr.toString(p)
    ++ " "
    ++ BinRel.toString(rel)
    ++ " "
    ++ Expr.toString(q)
  | Implies(p, q) => toString(p) ++ {j| → |j} ++ toString(q)
  | Conj(p, q) => toString(p) ++ {j| ⋀ |j} ++ toString(q)
  | Disj(p, q) => toString(p) ++ {j| ⋁ |j} ++ toString(q)
  | Neg(p) => {j|¬ |j} ++ toString(p)
  | Lit(true) => "true"
  | Lit(false) => "false"
  | Hole(int) => "?" ++ string_of_int(int);
