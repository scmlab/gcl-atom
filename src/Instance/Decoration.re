open Rebase;
open Types.Instance;
// Markers

let mark = (type_, class_, range, instance) => {
  open Atom;
  let marker = instance.editor |> TextEditor.markBufferRange(range);
  let option = TextEditor.decorateMarkerOptions(~type_, ~class_, ());
  let decoration =
    instance.editor |> Atom.TextEditor.decorateMarker(marker, option);
  instance.decorations = Array.concat(instance.decorations, [|decoration|]);
};

let markLineSpecSoft = mark("highlight", "highlight-spec-soft");
let markLineSpecHard = mark("highlight", "highlight-spec-hard");

let siteRange = (site, instance) => {
  switch (site) {
  | Response.Error.Site.Global(range) => range
  | Local(range, i) =>
    open Atom.Range;
    open Response.Specification;
    let specs = instance.specifications |> Array.filter(spec => spec.id == i);

    specs[0]
    |> Option.mapOr(
         spec =>
           range
           |> translate(start(spec.range), start(spec.range))
           // down by 1 line
           |> translate(Atom.Point.make(1, 0), Atom.Point.make(1, 0)),
         range,
       );
  };
};

let overlay =
    (
      text,
      class_,
      tail: bool,
      translation: (int, int),
      range: Atom.Range.t,
      instance,
    ) => {
  open Atom;
  open Webapi.Dom;
  // create an element for the overlay
  let element = Webapi.Dom.document |> Document.createElement("div");
  Element.setInnerHTML(element, text);
  element |> Element.classList |> DomTokenList.add(class_);

  // adjusting the position of the overlay
  // setStyle is not supported by Reason Webapi for the moment, so we use setAttribute instead

  let (y, x) = translation;
  let left = x;
  let top = float_of_int(y - 1) *. 1.5;

  element
  |> Element.setAttribute(
       "style",
       "left: "
       ++ string_of_int(left)
       ++ "ex; top: "
       ++ Js.Float.toString(top)
       ++ "em",
     );

  // decorate
  let marker = instance.editor |> TextEditor.markBufferRange(range);
  let option =
    TextEditor.decorateMarkerOptions(
      ~type_="overlay",
      ~position=tail ? "tail" : "head",
      ~item=Element.unsafeAsHtmlElement(element),
      (),
    );
  let decoration =
    instance.editor |> TextEditor.decorateMarker(marker, option);
  instance.decorations = Array.concat(instance.decorations, [|decoration|]);
};

let overlaySpec = (text, range: Atom.Range.t, instance) => {
  overlay(text, "overlay-spec-text", false, (0, 1), range, instance);
};

let overlayError = (range: Atom.Range.t, instance) => {
  let length =
    instance.editor
    |> Atom.TextEditor.getTextInBufferRange(range)
    |> Js.String.length;
  let text = Js.String.repeat(length, "&nbsp;");
  overlay(text, "overlay-error", true, (0, 0), range, instance);
};

let markSpec = (spec: Response.Specification.t, instance) => {
  open Response.Specification;
  open Atom;
  // let {hardness, pre, post, _, range} = spec;

  let start = Range.start(spec.range);
  let start = Range.make(start, Point.translate(Point.make(0, 2), start));
  let end_ = Range.end_(spec.range);
  let end_ = Range.make(Point.translate(Point.make(0, -2), end_), end_);

  switch (spec.hardness) {
  | Hard => markLineSpecHard(start, instance)
  | Soft => markLineSpecSoft(start, instance)
  };

  let trim = s =>
    if (String.length(s) > 77) {
      String.sub(~from=0, ~length=73, s) ++ " ...";
    } else {
      s;
    };

  let pre = trim(Expr.toString(spec.pre));
  let post = trim(Expr.toString(spec.post));
  overlaySpec(pre, start, instance);
  overlaySpec(post, end_, instance);
  markLineSpecSoft(end_, instance);
};

let markSite = (site, instance) => {
  let range = instance |> siteRange(site);
  overlayError(range, instance);
  mark("line-number", "line-number-error", range, instance);
  Async.resolve([]);
};
//
// let markSite2 = (site, instance) => {
//   Response.Error.Site.(
//     switch (site) {
//     | Global(range) => highlightError(range, instance)
//     // mark("line-number", "line-number-error", range, instance);
//     | Local(range, _i) => highlightError(range, instance)
//     // mark("line-number", "line-number-error", range, instance);
//     }
//   );
//   Async.resolve([]);
// };
// destroy all decorations
let destroyAll = instance =>
  instance.decorations |> Array.forEach(Atom.Decoration.destroy);
