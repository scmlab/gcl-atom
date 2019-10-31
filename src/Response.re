open Rebase;

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

  let point: decoder(Atom.Point.t) =
    json =>
      Atom.Point.make(
        field("line", int, json) - 1,
        field("column", int, json) - 1,
      );

  let range: decoder(Atom.Range.t) =
    field("tag", string)
    |> andThen(
         fun
         | "Loc" =>
           field("contents", json => {
             let x = json |> field("start", point);
             let y = json |> field("end", point);
             Atom.Point.(
               Atom.Range.make(
                 make(row(x), column(x)),
                 make(row(y), column(y)),
               )
             );
           })
         | _ => (
             _ =>
               Atom.Range.make(Atom.Point.make(0, 0), Atom.Point.make(0, 0))
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
