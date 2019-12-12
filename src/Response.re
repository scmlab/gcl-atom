open Json.Decode;
open Decoder;
module Error = {
  module Convert = {
    type t =
      | MissingBound
      | MissingAssertion
      | ExcessBound
      | MissingPostcondition
      | DigHole
      | Panic(string);

    let decode: decoder(t) =
      sum(
        fun
        | "MissingBound" => Contents(_ => MissingBound)
        | "MissingAssertion" => Contents(_ => MissingAssertion)
        | "ExcessBound" => Contents(_ => ExcessBound)
        | "MissingPostcondition" => TagOnly(_ => MissingPostcondition)
        | "DigHole" => Contents(_ => DigHole)
        | "Panic" => Contents(json => Panic(json |> string))
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

    let toRange =
      fun
      | Global(range) => range
      | Local(range, _) => range;

    let toString = site => {
      let rangeToString = range => {
        Atom.Range.(
          Atom.Point.(
            string_of_int(row(start(range)))
            ++ ":"
            ++ string_of_int(column(start(range)))
            ++ "-"
            ++ string_of_int(row(end_(range)))
            ++ ":"
            ++ string_of_int(column(end_(range)))
          )
        );
      };
      switch (site) {
      | Global(range) => "at " ++ rangeToString(range)
      | Local(range, i) =>
        "at " ++ rangeToString(range) ++ " in #" ++ string_of_int(i)
      };
    };
  };

  type kind =
    | LexicalError
    | SyntacticError(string)
    | ConvertError(Convert.t);

  let decodeKind: decoder(kind) =
    sum(
      fun
      | "LexicalError" => TagOnly(_ => LexicalError)
      | "SyntacticError" =>
        Contents(
          pair(range, string) |> map(((_, msg)) => SyntacticError(msg)),
        )
      | "ConvertError" =>
        Contents(json => ConvertError(json |> Convert.decode))
      | tag => raise(DecodeError("Unknown constructor: " ++ tag)),
    );

  type t =
    | Error(Site.t, kind);

  let decode: decoder(t) =
    pair(Site.decode, decodeKind)
    |> map(((site, kind)) => Error(site, kind));
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
  | Error(array(Error.t))
  | OK(array(Body.ProofObligation.t), array(Specification.t))
  | Resolve(int)
  | UnknownResponse(Js.Json.t);

let decode: decoder(t) =
  sum(
    fun
    | "Error" =>
      Contents(array(Error.decode) |> map(errors => Error(errors)))
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
