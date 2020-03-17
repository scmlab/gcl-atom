open Rebase;
open Base;
open State;
open Response.Specification;

let fromCursorPosition = state => {
  open Atom;

  let cursor = Atom.TextEditor.getCursorBufferPosition(state.editor);
  // find the smallest hole containing the cursor
  let smallestHole = ref(None);
  state.specifications
  |> Array.filter(spec =>
       Range.containsPoint(cursor, Loc.toRange(spec.loc))
     )
  |> Array.forEach(spec =>
       switch (smallestHole^) {
       | None => smallestHole := Some(spec)
       | Some(spec') =>
         if (Range.containsRange(
               Loc.toRange(spec.loc),
               Loc.toRange(spec'.loc),
             )) {
           smallestHole := Some(spec);
         }
       }
     );

  smallestHole^;
};

let getPayloadRange = (spec, state) => {
  open Atom;

  let startingRow = Point.row(Range.start(Loc.toRange(spec.loc))) + 1;
  let endingRow = Point.row(Range.end_(Loc.toRange(spec.loc))) - 1;

  let start =
    state.editor
    |> TextEditor.getBuffer
    |> TextBuffer.rangeForRow(startingRow, true)
    |> Range.start;
  let end_ =
    state.editor
    |> TextEditor.getBuffer
    |> TextBuffer.rangeForRow(endingRow, true)
    |> Range.end_;
  Range.make(start, end_);
};

let getPayload = (spec, state) => {
  open Atom;
  // return the text in the targeted hole
  let innerRange = getPayloadRange(spec, state);
  state.editor
  |> TextEditor.getBuffer
  |> TextBuffer.getTextInRange(innerRange);
};

let resolve = (i, state) => {
  open Atom;

  let specs = state.specifications |> Array.filter(spec => spec.id == i);
  specs[0]
  |> Option.forEach(spec => {
       let payload = getPayload(spec, state);
       Js.log2("!!!! [ payload ]", payload);
       let start = Range.start(Loc.toRange(spec.loc));

       state.editor
       |> TextEditor.getBuffer
       |> TextBuffer.delete(Loc.toRange(spec.loc))
       |> ignore;

       state.editor
       |> TextEditor.getBuffer
       |> TextBuffer.insert(start, Js.String.trim(payload))
       |> ignore;
     });
  Promise.resolved();
};

// NOTE: move this somewhere else
module Site = {
  open Response.Error.Site;
  let toLoc = (site, specifications) => {
    switch (site) {
    | Global(loc) => loc
    | Local(loc, i) =>
      let specs = specifications |> Array.filter(spec => spec.id == i);

      specs[0]
      |> Option.mapOr(
           spec =>
             spec.loc |> Loc.translate(loc) |> Loc.translateBy(1, 0, 1, 0),
           loc,
         );
    };
  };
  let toRange = (site, specifications) =>
    toLoc(site, specifications) |> Loc.toRange;
};

// rewrite "?" to "{!!}"
let digHole = (site, state) => {
  let range = Site.toRange(site, state.specifications);
  open Atom;
  let start = Range.start(range);
  // add indentation to the hole
  let indent = Js.String.repeat(Point.column(start), " ");
  let holeText = "{!\n" ++ indent ++ "\n" ++ indent ++ "!}";
  let holeRange =
    Range.make(start, Point.translate(start, Point.make(0, 1)));
  state.editor
  |> TextEditor.setTextInBufferRange(holeRange, holeText)
  |> ignore;
  // set the cursor inside the hole
  let cursorPos = Point.translate(start, Point.make(1, 0));
  state.editor |> TextEditor.setCursorBufferPosition(cursorPos);
  Promise.resolved();
};

let insert = (lineNo, expr, state) => {
  open Atom;

  let assertion = "{ " ++ Syntax.Expr.toString(expr) ++ " }\n";

  // set the cursor at the line
  let point = Point.make(lineNo - 1, 0);
  TextEditor.setCursorScreenPosition(point, state.editor);
  // insert the assertion
  TextEditor.insertText(assertion, state.editor) |> ignore;
};
