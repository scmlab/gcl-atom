open Rebase;

module React = {
  open ReasonReact;

  let manyIn = (elem, ~props=ReactDOMRe.domProps()) =>
    ReactDOMRe.createDOMElementVariadic(elem, ~props);

  let manyInFragment = ReactDOMRe.createElement(ReasonReact.fragment);

  let sepBy = (sep: reactElement, item: list(reactElement)) =>
    switch (item) {
    | [] => <> </>
    | [x] => x
    | [x, ...xs] =>
      {
        Array.fromList([x, ...List.map(i => <> sep i </>, xs)]);
      }
      |> manyIn("span")
    };
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
