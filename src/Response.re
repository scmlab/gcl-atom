open Json.Decode;
open Decoder;

module SyntaxError = {
  type t =
    | MissingBound(Atom.Range.t)
    | MissingAssertion(Atom.Range.t)
    | ExcessBound(Atom.Range.t)
    | MissingPostcondition
    | DigHole(Atom.Range.t)
    | Panic(string);

  let decode: decoder(t) =
    fields(
      fun
      | "MissingBound" => Contents(json => MissingBound(json |> range))
      | "MissingAssertion" =>
        Contents(json => MissingAssertion(json |> range))
      | "ExcessBound" => Contents(json => ExcessBound(json |> range))
      | "MissingPostcondition" => TagOnly(_ => MissingPostcondition)
      | "DigHole" => Contents(json => DigHole(json |> range))
      | "Panic" => Contents(json => Panic(json |> string))
      | tag => raise(DecodeError("Unknown constructor: " ++ tag)),
    );
};

module Specification = {
  type hardness =
    | Hard
    | Soft;

  type t =
    | Specification(hardness, Pred.t, Pred.t, Atom.Range.t);

  let decodeHardness: decoder(hardness) =
    string
    |> map(
         fun
         | "Hard" => Hard
         | "Soft" => Soft
         | tag => raise(DecodeError("Unknown constructor: " ++ tag)),
       );
  let decode: decoder(t) =
    tuple4(decodeHardness, Pred.decode, Pred.decode, range)
    |> map(((h, p, q, loc)) => Specification(h, p, q, loc));
};

type t =
  | ParseError(array((Atom.Point.t, string)))
  | SyntaxError(SyntaxError.t)
  | OK(array(Body.ProofObligation.t), array(Specification.t))
  | UnknownResponse(Js.Json.t);

let decode: decoder(t) =
  fields(
    fun
    | "ParseError" =>
      Contents(array(pair(point, string)) |> map(a => ParseError(a)))
    | "SyntaxError" =>
      Contents(json => SyntaxError(json |> SyntaxError.decode))
    | "OK" =>
      Contents(
        pair(
          array(Body.ProofObligation.decode),
          array(Specification.decode),
        )
        |> map(((obs, specs)) => OK(obs, specs)),
      )
    | tag => raise(DecodeError("Unknown constructor: " ++ tag)),
  );
