open Rebase;

// Markers

let mark = (type_, class_, range, instance: Type.instance) => {
  open Atom;
  let marker = instance.editor |> TextEditor.markBufferRange(range);
  let option = TextEditor.decorateMarkerOptions(~type_, ~class_, ());
  let decoration =
    instance.editor |> Atom.TextEditor.decorateMarker(marker, option);
  instance.decorations = Array.concat(instance.decorations, [|decoration|]);
};

let markLineError = mark("line", "line-number-error");
let markLineSpecSoft = mark("highlight", "highlight-spec-soft");
let markLineSpecHard = mark("highlight", "highlight-spec-hard");
let highlightError = mark("highlight", "line-number-error");

// rewrite "?" to "{!!}"
let digHole = (range, instance: Type.instance) => {
  open Atom;
  let start = Range.start(range);
  let range' = Range.make(start, Point.translate(start, Point.make(0, 1)));
  instance.editor
  |> TextEditor.setTextInBufferRange(range', "{!\n\n!}")
  |> ignore;

  Async.resolve();
};

let overlay =
    (
      text,
      class_,
      tail: bool,
      translation: (int, int),
      range: Atom.Range.t,
      instance: Type.instance,
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

let overlaySpec = (text, range: Atom.Range.t, instance: Type.instance) => {
  overlay(text, "overlay-spec-text", false, (0, 1), range, instance);
};

let overlayError = (range: Atom.Range.t, instance: Type.instance) => {
  let length =
    instance.editor
    |> Atom.TextEditor.getTextInBufferRange(range)
    |> Js.String.length;
  let text = Js.String.repeat(length, "&nbsp;");
  overlay(text, "overlay-error", true, (0, 0), range, instance);
};

let markSpec = (spec: Response.Specification.t, instance: Type.instance) => {
  open Response.Specification;
  let Specification(hardness, pre, post, start, end_) = spec;

  switch (hardness) {
  | Hard => markLineSpecHard(start, instance)
  | Soft => markLineSpecSoft(start, instance)
  };

  overlaySpec(Pred.toString(pre), start, instance);
  overlaySpec(Pred.toString(post), end_, instance);
  markLineSpecSoft(end_, instance);
};

let markError = (point, instance) => {
  let range =
    Atom.Range.make(
      point,
      Atom.Point.make(Atom.Point.row(point), Atom.Point.column(point) + 1),
    );
  overlayError(range, instance);
  mark("line-number", "line-number-error", range, instance);
};

let markError' = (range, instance) => {
  overlayError(range, instance);
  mark("line-number", "line-number-error", range, instance);
};
