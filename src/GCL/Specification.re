open Json.Decode;
open Decoder;

type t = {
  id: int,
  pre: Syntax.Expr.t,
  post: Syntax.Expr.t,
  range: Atom.Range.t,
};

let decode: decoder(t) =
  json => {
    id: json |> field("specID", int),
    pre: json |> field("specPreCond", Syntax.Expr.decode),
    post: json |> field("specPostCond", Syntax.Expr.decode),
    range: json |> field("specLoc", range),
  };
