open Json.Decode;
open Decoder;

type t = {
  id: int,
  pre: Expr.t,
  post: Expr.t,
  range: Atom.Range.t,
};

let decode: decoder(t) =
  json => {
    id: json |> field("specID", int),
    pre: json |> field("specPreCond", Expr.decode),
    post: json |> field("specPostCond", Expr.decode),
    range: json |> field("specLoc", range),
  };
