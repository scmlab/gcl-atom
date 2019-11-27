open Json.Decode;
open Decoder;
module Error = {
  module TransformError = {
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

  module SyntacticError = {
    type t = {
      locations: array(Atom.Range.t),
      message: string,
    };

    let decode: decoder(t) =
      json => {
        locations: json |> field("synErrLocations", array(range)),
        message: json |> field("synErrMessage", string),
      };
  };
  type t =
    | LexicalError(Atom.Point.t)
    | SyntacticError(array(SyntacticError.t))
    | TransformError(TransformError.t);

  let decode: decoder(t) =
    fields(
      fun
      | "LexicalError" => Contents(json => LexicalError(json |> point))
      | "SyntacticError" =>
        Contents(
          array(SyntacticError.decode) |> map(pairs => SyntacticError(pairs)),
        )
      | "TransformError" =>
        Contents(json => TransformError(json |> TransformError.decode))
      | tag => raise(DecodeError("Unknown constructor: " ++ tag)),
    );
};

module Specification = {
  type hardness =
    | Hard
    | Soft;

  type t =
    | Specification(hardness, Pred.t, Pred.t, Atom.Range.t, Atom.Range.t);

  let decodeHardness: decoder(hardness) =
    string
    |> map(
         fun
         | "Hard" => Hard
         | "Soft" => Soft
         | tag => raise(DecodeError("Unknown constructor: " ++ tag)),
       );

  let decode: decoder(t) =
    json =>
      Specification(
        json |> field("specHardness", decodeHardness),
        json |> field("specPreCond", Pred.decode),
        json |> field("specPostCond", Pred.decode),
        json |> field("specStartLoc", range),
        json |> field("specEndLoc", range),
      );
};

type t =
  // | ParseError(array((Atom.Point.t, string)))
  | Error(Error.t)
  | OK(array(Body.ProofObligation.t), array(Specification.t))
  | UnknownResponse(Js.Json.t);

let decode: decoder(t) =
  fields(
    fun
    | "Error" => Contents(Error.decode |> map(e => Error(e)))
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
