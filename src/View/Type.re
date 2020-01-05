open! Rebase;
open Decoder;

module Base = {
  type t =
    | Int
    | Bool;

  let decode: Json.Decode.decoder(t) =
    Json.Decode.(
      string
      |> map(
           fun
           | "TInt" => Int
           | "TBool" => Bool
           | tag => raise(DecodeError("Unknown constructor: " ++ tag)),
         )
    );
  // sum(
  //   fun
  //   | "TInt" => TagOnly(_ => Int)
  //   | "TBool" => TagOnly(_ => Bool)
  //   | tag => raise(DecodeError("Unknown constructor: " ++ tag)),
  // )

  let toString =
    fun
    | Int => "Int"
    | Bool => "Bool";
};

type t =
  | Base(Base.t)
  | Array(t)
  | Func(t, t)
  | Var(int);

let rec decode: Json.Decode.decoder(t) =
  json =>
    json
    |> Json.Decode.(
         sum(
           fun
           | "TBase" => Contents(Base.decode |> map(x => Base(x)))
           | "TArray" => Contents(decode |> map(x => Array(x)))
           | "TFun" =>
             Contents(pair(decode, decode) |> map(((x, y)) => Func(x, y)))
           | "TVar" => Contents(int |> map(x => Var(x)))
           | tag => raise(DecodeError("Unknown constructor: " ++ tag)),
         )
       );

let rec toString =
  fun
  | Base(b) => Base.toString(b)
  | Array(t) => "Array " ++ toString(t)
  | Func(s, t) => toString(s) ++ " -> " ++ toString(t)
  | Var(i) => "Var " ++ string_of_int(i);
