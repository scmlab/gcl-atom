open! Rebase;
open Rebase.Fn;

type range = Atom.Range.t;

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
