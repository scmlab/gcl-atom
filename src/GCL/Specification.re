open Json.Decode;
open Syntax;
open! Decoder;

type t = {
  id: int,
  pre: Syntax.Expr.t,
  post: Syntax.Expr.t,
  loc,
};

let decode: decoder(t) =
  json => {
    id: json |> field("specID", int),
    pre: json |> field("specPreCond", Syntax.Expr.decode),
    post: json |> field("specPostCond", Syntax.Expr.decode),
    loc: json |> field("specLoc", Loc.decode),
  };
