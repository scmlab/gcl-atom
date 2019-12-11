open Json.Decode;
open Decoder;
module Error = {
  module Syntax = {
    module Transform = {
      type t =
        | MissingBound(Atom.Range.t)
        | MissingAssertion(Atom.Range.t)
        | ExcessBound(Atom.Range.t)
        | MissingPostcondition
        | DigHole(Atom.Range.t)
        | Panic(string);

      let decode: decoder(t) =
        sum(
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

    module Syntactic = {
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
      | SyntacticError(array(Syntactic.t))
      | TransformError(Transform.t);

    let decode: decoder(t) =
      sum(
        fun
        | "LexicalError" => Contents(json => LexicalError(json |> point))
        | "SyntacticError" =>
          Contents(
            array(Syntactic.decode) |> map(pairs => SyntacticError(pairs)),
          )
        | "TransformError" =>
          Contents(json => TransformError(json |> Transform.decode))
        | tag => raise(DecodeError("Unknown constructor: " ++ tag)),
      );
  };

  module Site = {
    type t =
      | Global(Atom.Range.t)
      | Local(Atom.Range.t, int);

    let decode: decoder(t) =
      sum(
        fun
        | "Global" => Contents(json => Global(json |> range))
        | "Local" =>
          Contents(pair(range, int) |> map(((r, i)) => Local(r, i)))
        | tag => raise(DecodeError("Unknown constructor: " ++ tag)),
      );
  };

  type t =
    | SyntaxError(Syntax.t);

  let decode: decoder(t) =
    sum(
      fun
      | "SyntaxError" => Contents(json => SyntaxError(json |> Syntax.decode))
      | tag => raise(DecodeError("Unknown constructor: " ++ tag)),
    );
};

module Specification = {
  type hardness =
    | Hard
    | Soft;

  type t = {
    id: int,
    hardness,
    pre: Pred.t,
    post: Pred.t,
    // lastStmtRange: option(Atom.Range.t),
    range: Atom.Range.t,
  };

  let decodeHardness: decoder(hardness) =
    string
    |> map(
         fun
         | "Hard" => Hard
         | "Soft" => Soft
         | tag => raise(DecodeError("Unknown constructor: " ++ tag)),
       );

  let decode: decoder(t) =
    json => {
      id: json |> field("specID", int),
      hardness: json |> field("specHardness", decodeHardness),
      pre: json |> field("specPreCond", Pred.decode),
      post: json |> field("specPostCond", Pred.decode),
      // lastStmtRange: json |> field("specLastStmt", optional(range)),
      range: json |> field("specLoc", range),
    };
};

type t =
  | Error(array((Error.Site.t, Error.t)))
  | OK(array(Body.ProofObligation.t), array(Specification.t))
  | Resolve(int)
  | UnknownResponse(Js.Json.t);

let decode: decoder(t) =
  sum(
    fun
    | "Error" =>
      Contents(
        array(pair(Error.Site.decode, Error.decode))
        |> map(pairs => Error(pairs)),
      )
    | "OK" =>
      Contents(
        pair(
          array(Body.ProofObligation.decode),
          array(Specification.decode),
        )
        |> map(((obs, specs)) => OK(obs, specs)),
      )
    | "Resolve" => Contents(int |> map(i => Resolve(i)))
    | tag => raise(DecodeError("Unknown constructor: " ++ tag)),
  );
