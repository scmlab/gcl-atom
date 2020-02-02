open Json.Decode;
open Base;
open! Decoder;

type t = {
  id: int,
  pre: Syntax.Pred.t,
  post: Syntax.Pred.t,
  loc,
};

let decode: decoder(t) =
  json => {
    id: json |> field("specID", int),
    pre: json |> field("specPreCond", Syntax.Pred.decode),
    post: json |> field("specPostCond", Syntax.Pred.decode),
    loc: json |> field("specLoc", Loc.decode),
  };
