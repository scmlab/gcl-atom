open! Rebase;
open Decoder;

type t =
  | Int
  | Bool
  | Array(t)
  | Func(t, t)
  | Var(int);

let rec decode: Json.Decode.decoder(t) =
  json =>
    json
    |> Json.Decode.(
         sum(
           fun
           | "TInt" => TagOnly(_ => Int)
           | "TBool" => TagOnly(_ => Bool)
           | "TArray" => Contents(decode |> map(x => Array(x)))
           | "TFun" =>
             Contents(pair(decode, decode) |> map(((x, y)) => Func(x, y)))
           | "TVar" => Contents(int |> map(x => Var(x)))
           | tag => raise(DecodeError("Unknown constructor: " ++ tag)),
         )
       );

let rec toString =
  fun
  | Int => "Int"
  | Bool => "Bool"
  | Array(t) => "Array " ++ toString(t)
  | Func(s, t) => toString(s) ++ " -> " ++ toString(t)
  | Var(i) => "Var " ++ string_of_int(i);
