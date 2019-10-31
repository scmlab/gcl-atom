open Rebase;

type syntaxError =
  | MissingBound(Atom.Range.t)
  | MissingAssertion(Atom.Range.t)
  | ExcessBound(Atom.Range.t);

type proofObligation = string;

type t =
  | OK
  | ParseError(array((Atom.Point.t, string)))
  | SyntaxError(syntaxError)
  | ProofObligations(array(proofObligation))
  | UnknownResponse(Js.Json.t);

module Decode = {
  open Json.Decode;

  let fields = decoder =>
    field("tag", string) |> andThen(tag => field("contents", decoder(tag)));

  let point: decoder(Atom.Point.t) =
    json =>
      Atom.Point.make(
        field("line", int, json) - 1,
        field("column", int, json) - 1,
      );

  let range: decoder(Atom.Range.t) =
    fields(
      fun
      | "Loc" => (
          json => {
            let x = json |> field("start", point);
            let y = json |> field("end", point);
            Atom.Point.(
              Atom.Range.make(
                make(row(x), column(x)),
                make(row(y), column(y)),
              )
            );
          }
        )
      | _ => (
          _ => Atom.Range.make(Atom.Point.make(0, 0), Atom.Point.make(0, 0))
        ),
    );

  let proofObligation: decoder(proofObligation) = string;

  // response
  let ok: decoder(t) = _ => OK;

  let parseError: decoder(t) =
    array(pair(point, string)) |> map(a => ParseError(a));

  let syntaxError: decoder(syntaxError) =
    fields(
      fun
      | "MissingBound" => (json => MissingBound(json |> range))
      | "MissingAssertion" => (json => MissingAssertion(json |> range))
      | "ExcessBound" => (json => ExcessBound(json |> range))
      | tag => raise(DecodeError("Unknown constructor: " ++ tag)),
    );

  let response: decoder(t) =
    fields(
      fun
      | "OK" => ok
      | "ParseError" => parseError
      | "SyntaxError" => (json => SyntaxError(json |> syntaxError))
      | "ProofObligations" => (
          json => ProofObligations(json |> array(proofObligation))
        )
      | tag => raise(DecodeError("Unknown constructor: " ++ tag)),
    );
};

let parse: string => option(t) =
  data => data |> Json.parse |> Option.map(Decode.response);
let test: string => option(Js.Json.t) = data => data |> Json.parse;
