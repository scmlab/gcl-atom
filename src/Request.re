type t =
  | Load(string)
  | Refine(int, string);

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
      ]);
};

let encode: t => string = x => x |> Encode.request |> Json.stringify;
