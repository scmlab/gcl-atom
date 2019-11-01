open Rebase.Fn;
open Decoder;

// type lit =
//   | Num(int)
//   | Bool(bool);

type expr = Js.Json.t;
//   | Var(string)
//   | Const(string)
//   | Lit(lit)
//   | Op(expr, array(expr))
//   | Hole(int, array(subst))
// and subst = Js.Dict.t(expr);

module BinRel = {
  type t =
    | Eq
    | LEq
    | GEq
    | LTh
    | GTh;

  let decode: Json.Decode.decoder(t) =
    Json.Decode.(
      fields(
        fun
        | "Eq" => TagOnly(_ => Eq)
        | "LEq" => TagOnly(_ => LEq)
        | "GEq" => TagOnly(_ => GEq)
        | "LTh" => TagOnly(_ => LTh)
        | "GTh" => TagOnly(_ => GTh)
        | tag => raise(DecodeError("Unknown constructor: " ++ tag)),
      )
    );

  let toString =
    fun
    | Eq => "="
    | LEq => "<="
    | GEq => ">="
    | LTh => ">"
    | GTh => "<";
};

type t =
  | Term(BinRel.t, expr, expr)
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
               tuple3(BinRel.decode, id, id)
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
    BinRel.toString(rel)
    ++ " "
    ++ Js.Json.stringify(p)
    ++ " "
    ++ Js.Json.stringify(q)
  | Implies(p, q) => toString(p) ++ {j| → |j} ++ toString(q)
  | Conj(p, q) => toString(p) ++ {j| ⋀ |j} ++ toString(q)
  | Disj(p, q) => toString(p) ++ {j| ⋁ |j} ++ toString(q)
  | Neg(p) => {j|¬ |j} ++ toString(p)
  | Lit(true) => "true"
  | Lit(false) => "false"
  | Hole(int) => "?" ++ string_of_int(int);
