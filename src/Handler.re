open Rebase;

let mark = (type_, class_, range, instance: Type.instance) => {
  open Atom;
  let marker = instance.editor |> TextEditor.markBufferRange(range);
  let option = TextEditor.decorateMarkerOptions(~type_, ~class_, ());
  let decoration =
    instance.editor |> Atom.TextEditor.decorateMarker(marker, option);
  instance.decorations = Array.concat(instance.decorations, [|decoration|]);
};

let overlay = (text, range: Atom.Range.t, instance: Type.instance) => {
  open Atom;
  open Webapi.Dom;
  // create an element for the overlay
  let element = Webapi.Dom.document |> Document.createElement("div");
  Element.setInnerHTML(element, text);
  element |> Element.classList |> DomTokenList.add("marker-spec-text");

  // adjusting the position of the overlay
  // setStyle is not supported by Reason Webapi for the moment, so we use setAttribute instead
  element |> Element.setAttribute("style", "left: 3ex");
  // decorate
  let range' =
    range |> Atom.Range.translate(Point.make(-1, 0), Point.make(-1, 0));
  let marker = instance.editor |> TextEditor.markBufferRange(range');
  let option =
    TextEditor.decorateMarkerOptions(
      ~type_="overlay",
      ~position="head",
      ~item=Element.unsafeAsHtmlElement(element),
      (),
    );
  let decoration =
    instance.editor |> TextEditor.decorateMarker(marker, option);
  instance.decorations = Array.concat(instance.decorations, [|decoration|]);
};

let markLineError = mark("line", "marker-error");
let markLineSpecSoft = mark("highlight", "marker-spec-soft");
let markLineSpecHard = mark("highlight", "marker-spec-hard");
let highlightError = mark("highlight", "marker-error");

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

let markSpec = (spec: Response.Specification.t, instance: Type.instance) => {
  open Response.Specification;
  let Specification(hardness, pre, post, start, end_) = spec;

  switch (hardness) {
  | Hard => markLineSpecHard(start, instance)
  | Soft => markLineSpecSoft(start, instance)
  };

  overlay(Pred.toString(pre), start, instance);
  overlay(Pred.toString(post), end_, instance);
  markLineSpecSoft(end_, instance);
};
