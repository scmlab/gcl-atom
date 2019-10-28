open Rebase;

type pos = {
  filepath: string,
  offset: int,
  line: int,
  column: int,
};

type t =
  | OK
  | ParseError(Atom.Point.t, string);

module Decode = {
  open Json.Decode;

  // pos
  let pos: decoder(pos) =
    json => {
      filepath: json |> field("filepath", string),
      offset: json |> field("offset", int),
      line: json |> field("line", int),
      column: json |> field("column", int),
    };

  // response
  let ok: decoder(t) = _ => OK;
  let parseError: decoder(t) =
    pair(pos, string)
    |> map(((x, y)) => ParseError(Atom.Point.make(x.line, x.column), y));

  let response: decoder(t) =
    field("tag", string)
    |> andThen(
         fun
         | "OK" => ok
         | "ParseError" => field("contents", parseError)
         | _ => ok,
       );
};

let parse: string => option(t) =
  data => data |> Json.parse |> Option.map(Decode.response);
let test: string => option(Js.Json.t) = data => data |> Json.parse;
