open! Rebase;
open Rebase.Fn;

type range = Atom.Range.t;

[@bs.val] external _stringify: Js.Json.t => string = "JSON.stringify";

module Decode = {
  open Json.Decode;

  type fieldType('a) =
    | Contents(decoder('a))
    | TagOnly(decoder('a));

  let sum = decoder =>
    field("tag", string)
    |> andThen(tag =>
         switch (decoder(tag)) {
         | Contents(d) => field("contents", d)
         | TagOnly(d) => d
         }
       );

  let maybe: decoder('a) => decoder(option('a)) =
    decoder =>
      sum(
        fun
        | "Just" => Contents(json => Some(decoder(json)))
        | _ => TagOnly(_ => None),
      );

  let tuple5 = (decodeA, decodeB, decodeC, decodeD, decodeE, json) =>
    if (Js.Array.isArray(json)) {
      let source: array(Js.Json.t) = Obj.magic(json: Js.Json.t);
      let length = Js.Array.length(source);
      if (length == 5) {
        try (
          decodeA(Js.Array.unsafe_get(source, 0)),
          decodeB(Js.Array.unsafe_get(source, 1)),
          decodeC(Js.Array.unsafe_get(source, 2)),
          decodeD(Js.Array.unsafe_get(source, 3)),
          decodeE(Js.Array.unsafe_get(source, 4)),
        ) {
        | DecodeError(msg) => raise(DecodeError(msg ++ "\n\tin tuple5"))
        };
      } else {
        raise(
          DecodeError(
            {j|Expected array of length 5, got array of length $length|j},
          ),
        );
      };
    } else {
      raise(DecodeError("Expected array, got " ++ _stringify(json)));
    };
};

module React = {
  open ReasonReact;

  let manyIn = (elem, ~props=ReactDOMRe.domProps()) =>
    ReactDOMRe.createDOMElementVariadic(elem, ~props);

  let manyInFragment = ReactDOMRe.createElement(ReasonReact.fragment);

  let sepBy' = (sep: reactElement, item: list(reactElement)) =>
    switch (item) {
    | [] => <> </>
    | [x] => x
    | [x, ...xs] =>
      {
        Array.fromList([x, ...List.map(i => <> sep i </>, xs)]);
      }
      |> manyIn("span")
    };
  let sepBy = (sep: reactElement) => List.fromArray >> sepBy'(sep);

  let enclosedBy =
      (front: reactElement, back: reactElement, item: reactElement) =>
    <> front {string(" ")} item {string(" ")} back </>;

  let when_ = (p, className) => p ? " " ++ className : "";
  let showWhen =
    fun
    | true => ""
    | false => " hidden";
};

module JsError = {
  let toString = (_e: Js.Exn.t) => {
    %raw
    "_e.toString()";
  };
};

module Result = {
  type t('a, 'e) = result('a, 'e);
  let every = (xs: array(t('a, 'e))): t(array('a), 'e) =>
    Array.reduce(
      (acc, x) =>
        switch (acc, x) {
        | (Ok(xs), Ok(v)) =>
          xs |> Js.Array.push(v) |> ignore;
          Ok(xs);
        | (_, Error(e)) => Error(e)
        | (Error(e), _) => Error(e)
        },
      Ok([||]),
      xs,
    );
};

// module Promise = {
//   type t('a, 'e) = result('a, 'e);
//   let every = (xs: array(t('a, 'e))): t(array('a), 'e) =>
//     Array.reduce(
//       (acc, x) =>
//         switch (acc, x) {
//         | (Ok(xs), Ok(v)) =>
//           xs |> Js.Array.push(v) |> ignore;
//           Ok(xs);
//         | (_, Error(e)) => Error(e)
//         | (Error(e), _) => Error(e)
//         },
//       Ok([||]),
//       xs,
//     );
// };
