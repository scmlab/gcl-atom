open Json.Decode;
open Decoder;

type t = {
  id: int,
  pre: Expr.t,
  post: Expr.t,
  // lastStmtRange: option(Atom.Range.t),
  range: Atom.Range.t,
};

let decode: decoder(t) =
  json => {
    id: json |> field("specID", int),
    pre: json |> field("specPreCond", Expr.decode),
    post: json |> field("specPostCond", Expr.decode),
    // lastStmtRange: json |> field("specLastStmt", optional(range)),
    range: json |> field("specLoc", range),
  };
