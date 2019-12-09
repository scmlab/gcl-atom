open Rebase;
open Type.Instance;
// Markers

let mark = (type_, class_, range, instance) => {
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
let digHole = (range, instance) => {
  open Atom;
  let start = Range.start(range);
  // add indentation to the hole
  let indent = Js.String.repeat(Point.column(start), " ");
  let holeText = "{!\n" ++ indent ++ "\n" ++ indent ++ "!}";
  let holeRange =
    Range.make(start, Point.translate(start, Point.make(0, 1)));
  instance.editor
  |> TextEditor.setTextInBufferRange(holeRange, holeText)
  |> ignore;
  // set the cursor inside the hole
  let cursorPos = Point.translate(start, Point.make(1, 0));
  instance.editor |> TextEditor.setCursorBufferPosition(cursorPos);
  Async.resolve();
};

module Spec = {
  open Atom;
  open Response.Specification;
  let fromCursorPosition = instance => {
    let cursor = instance.editor |> Atom.TextEditor.getCursorBufferPosition;
    // find the smallest hole containing the cursor
    let smallestHole = ref(None);
    instance.specifications
    |> Array.filter(spec => Range.containsPoint(cursor, spec.range))
    |> Array.forEach(spec =>
         switch (smallestHole^) {
         | None => smallestHole := Some(spec)
         | Some(spec') =>
           if (Range.containsRange(spec.range, spec'.range)) {
             smallestHole := Some(spec);
           }
         }
       );

    smallestHole^;
  };

  let getPayloadRange = spec => {
    // return the text in the targeted hole
    let start = Point.translate(Range.start(spec.range), Point.make(1, 0));
    let end_ = Point.translate(Range.end_(spec.range), Point.make(0, -2));
    Range.make(start, end_);
  };

  let getPayload = (spec, instance) => {
    // return the text in the targeted hole
    let innerRange = getPayloadRange(spec);
    instance.editor
    |> TextEditor.getBuffer
    |> TextBuffer.getTextInRange(innerRange);
  };

  let resolve = (i, instance) => {
    let specs = instance.specifications |> Array.filter(spec => spec.id == i);
    specs[0]
    |> Option.forEach(spec => {
         // delete the rows containing the hole boundaries
         let startingRow = Point.row(Range.start(spec.range));
         let endingRow = Point.row(Range.end_(spec.range));

         instance.editor
         |> TextEditor.getBuffer
         |> TextBuffer.deleteRow(endingRow)
         |> ignore;
         instance.editor
         |> TextEditor.getBuffer
         |> TextBuffer.deleteRow(startingRow)
         |> ignore;
       });
  };
};

// let getSpecPayload = (cursor, instance) => {
//   // switch (spec.lastStmtRange) {
//   // | None =>
//   //   instance.editor
//   //   |> TextEditor.getBuffer
//   //   |> TextBuffer.setTextInRange(spec.range, "")
//   //   |> ignore;
//   //   Async.resolve();
//   // | Some(stmtRange) =>
//   //   let indent =
//   //     Js.String.repeat(Point.column(Range.start(stmtRange)), " ");
//   //   let stmt =
//   //     "\n"
//   //     ++ indent
//   //     ++ TextEditor.getTextInBufferRange(stmtRange, instance.editor);
//   //   // paste rows
//   //   let newRange =
//   //     Range.make(Range.end_(spec.range), Range.end_(spec.range));
//   //   instance.editor
//   //   |> TextEditor.getBuffer
//   //   |> TextBuffer.setTextInRange(newRange, stmt)
//   //   |> ignore;
//   //   // delete rows
//   //   instance.editor
//   //   |> TextEditor.getBuffer
//   //   |> TextBuffer.deleteRows(
//   //        Point.row(Range.start(stmtRange)),
//   //        Point.row(Range.end_(stmtRange)),
//   //      )
//   //   |> ignore;
//   //   // move the cursor up a bit
//   //   instance.editor |> TextEditor.moveLeft;
//   //
//   //   Async.resolve();
//   // }
// };

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

  let pre = trim(Pred.toString(spec.pre));
  let post = trim(Pred.toString(spec.post));
  overlaySpec(pre, start, instance);
  overlaySpec(post, end_, instance);
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
