open Rebase;
open Types.Instance;
open Specification;

let fromCursorPosition = instance => {
  open Atom;

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

let getPayloadRange = (spec, instance) => {
  open Atom;

  // return the text in the targeted hole
  let start = Point.translate(Range.start(spec.range), Point.make(1, 0));
  let end_ =
    instance.editor
    |> TextEditor.getBuffer
    |> TextBuffer.rangeForRow(Point.row(Range.end_(spec.range)) - 1, true)
    |> Range.end_;
  Range.make(start, end_);
};

let getPayload = (spec, instance) => {
  open Atom;
  // return the text in the targeted hole
  let innerRange = getPayloadRange(spec, instance);
  instance.editor
  |> TextEditor.getBuffer
  |> TextBuffer.getTextInRange(innerRange);
};

let resolve = (i, instance) => {
  open Atom;

  let specs = instance.specifications |> Array.filter(spec => spec.id == i);
  specs[0]
  |> Option.forEach(spec => {
       let payload = getPayload(spec, instance);
       let start = Range.start(spec.range);

       instance.editor
       |> TextEditor.getBuffer
       |> TextBuffer.delete(spec.range)
       |> ignore;

       instance.editor
       |> TextEditor.getBuffer
       |> TextBuffer.insert(start, Js.String.trim(payload))
       |> ignore;
     });
  Promise.resolved();
};

// rewrite "?" to "{!!}"
let digHole = (site, instance) => {
  let range = instance.specifications |> ErrorSite.toRange(site);
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
  Promise.resolved();
};
