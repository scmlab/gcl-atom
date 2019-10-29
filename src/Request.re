type t =
  | Load(string);

module Encode = {
  open Json.Encode;

  let request: encoder(t) =
    fun
    | Load(filepath) =>
      object_([("tag", string("Load")), ("contents", string(filepath))]);
};

let encode: t => string = x => x |> Encode.request |> Json.stringify;
