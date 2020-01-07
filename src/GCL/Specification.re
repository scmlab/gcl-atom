open Json.Decode;
open Decoder;

type hardness =
  | Hard
  | Soft;

type t = {
  id: int,
  hardness,
  pre: Expr.t,
  post: Expr.t,
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
    pre: json |> field("specPreCond", Expr.decode),
    post: json |> field("specPostCond", Expr.decode),
    // lastStmtRange: json |> field("specLastStmt", optional(range)),
    range: json |> field("specLoc", range),
  };
