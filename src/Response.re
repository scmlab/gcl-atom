open Rebase;

type pos = {
  filepath: string,
  offset: int,
  line: int,
  column: int,
};

// type syntaxError =

type t =
  | OK
  | ParseError(array((Atom.Point.t, string)))
  // | SyntaxError(syntaxError)
  | UnknownResponse(Js.Json.t);

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

  let parseErrorPair: decoder((Atom.Point.t, string)) =
    pair(pos, string)
    |> map(((x, y)) => (Atom.Point.make(x.line - 1, x.column), y));

  let parseError: decoder(t) =
    array(parseErrorPair) |> map(a => ParseError(a));

  let response: decoder(t) =
    raw =>
      raw
      |> (
        field("tag", string)
        |> andThen(
             fun
             | "OK" => ok
             | "ParseError" => field("contents", parseError)
             | _ => (_ => UnknownResponse(raw)),
           )
      );
};

let parse: string => option(t) =
  data => data |> Json.parse |> Option.map(Decode.response);
let test: string => option(Js.Json.t) = data => data |> Json.parse;
