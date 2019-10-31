open Rebase;

type syntaxError =
  | MissingBound(Atom.Range.t)
  | MissingAssertion(Atom.Range.t)
  | ExcessBound(Atom.Range.t)
  | MissingPostcondition;

type proofObligation = string;

type t =
  | OK
  | ParseError(array((Atom.Point.t, string)))
  | SyntaxError(syntaxError)
  | ProofObligations(array(proofObligation))
  | UnknownResponse(Js.Json.t);

module Decode = {
  open Json.Decode;

  type fieldType('a) =
    | Contents(decoder('a))
    | TagOnly(decoder('a));

  let fields = decoder =>
    field("tag", string)
    |> andThen(tag =>
         switch (decoder(tag)) {
         | Contents(d) => field("contents", d)
         | TagOnly(d) => d
         }
       );

  let point: decoder(Atom.Point.t) =
    json =>
      Atom.Point.make(
        field("line", int, json) - 1,
        field("column", int, json) - 1,
      );

  let range: decoder(Atom.Range.t) =
    fields(
      fun
      | "Loc" =>
        Contents(
          json => {
            let x = json |> field("start", point);
            let y = json |> field("end", point);
            Atom.Point.(
              Atom.Range.make(
                make(row(x), column(x)),
                make(row(y), column(y)),
              )
            );
          },
        )
      | _ =>
        TagOnly(
          _ =>
            Atom.Range.make(Atom.Point.make(0, 0), Atom.Point.make(0, 0)),
        ),
    );

  let proofObligation: decoder(proofObligation) = string;

  let syntaxError: decoder(syntaxError) =
    fields(
      fun
      | "MissingBound" => Contents(json => MissingBound(json |> range))
      | "MissingAssertion" =>
        Contents(json => MissingAssertion(json |> range))
      | "ExcessBound" => Contents(json => ExcessBound(json |> range))
      | "MissingPostcondition" => TagOnly(_ => MissingPostcondition)
      | tag => raise(DecodeError("Unknown constructor: " ++ tag)),
    );

  // response
  let response: decoder(t) =
    fields(
      fun
      | "OK" => TagOnly(_ => OK)
      | "ParseError" =>
        Contents(array(pair(point, string)) |> map(a => ParseError(a)))
      | "SyntaxError" => Contents(json => SyntaxError(json |> syntaxError))
      | "ProofObligations" =>
        Contents(json => ProofObligations(json |> array(proofObligation)))
      | tag => raise(DecodeError("Unknown constructor: " ++ tag)),
    );
};

let parse: Js.Json.t => t = Decode.response;
