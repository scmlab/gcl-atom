open Rebase.Fn;
type t =
  | Load(string)
  | Refine(int, string)
  | InsertAssertion(int)
  | Debug;

module Encode = {
  open Json.Encode;

  let request: encoder(t) =
    fun
    | Load(filepath) =>
      object_([("tag", string("Load")), ("contents", string(filepath))])
    | Refine(id, payload) =>
      object_([
        ("tag", string("Refine")),
        ("contents", (id, payload) |> pair(int, string)),
      ])
    | InsertAssertion(n) =>
      object_([("tag", string("InsertAssertion")), ("contents", int(n))])
    | Debug => object_([("tag", string("Debug"))]);
};

let encode: t => string = Encode.request >> Json.stringify;
