open Rebase;

type pos = {
  filepath: string,
  offset: int,
  line: int,
  column: int,
};

type loc =
  | NoLoc
  | Loc(pos, pos);

type syntaxError =
  | MissingBound(Atom.Range.t)
  | MissingAssertion(Atom.Range.t)
  | ExcessBound(Atom.Range.t);

type t =
  | OK
  | ParseError(array((Atom.Point.t, string)))
  | SyntaxError(syntaxError)
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

  // loc
  let loc: decoder(loc) =
    field("tag", string)
    |> andThen(
         fun
         | "Loc" =>
           field("contents", json =>
             Loc(json |> field("start", pos), json |> field("end", pos))
           )
         | _ => (_ => NoLoc),
       );

  let point: decoder(Atom.Point.t) =
    pos |> map(x => Atom.Point.make(x.line - 1, x.column));

  let range: decoder(Atom.Range.t) =
    loc
    |> map(
         fun
         | NoLoc =>
           Atom.Range.make(Atom.Point.make(0, 0), Atom.Point.make(0, 0))
         | Loc(x, y) =>
           Atom.Range.make(
             Atom.Point.make(x.line - 1, x.column - 1),
             Atom.Point.make(y.line - 1, y.column - 1),
           ),
       );

  // response
  let ok: decoder(t) = _ => OK;

  let parseErrorPair: decoder((Atom.Point.t, string)) = pair(point, string);

  let parseError: decoder(t) =
    array(parseErrorPair) |> map(a => ParseError(a));

  let syntaxError: decoder(syntaxError) =
    field("tag", string)
    |> andThen(
         fun
         | "MissingBound" =>
           field("contents", json => MissingBound(json |> range))
         | "MissingAssertion" =>
           field("contents", json => MissingAssertion(json |> range))
         | "ExcessBound" =>
           field("contents", json => ExcessBound(json |> range))
         | tag => raise(DecodeError("Unknown constructor: " ++ tag)),
       );

  let response: decoder(t) =
    raw =>
      raw
      |> (
        field("tag", string)
        |> andThen(
             fun
             | "OK" => ok
             | "ParseError" => field("contents", parseError)
             | "SyntaxError" =>
               field("contents", json => SyntaxError(json |> syntaxError))
             | _ => (_ => UnknownResponse(raw)),
           )
      );
};

let parse: string => option(t) =
  data => data |> Json.parse |> Option.map(Decode.response);
let test: string => option(Js.Json.t) = data => data |> Json.parse;
