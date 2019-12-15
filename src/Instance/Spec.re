open Rebase;
open Type.Instance;
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
  Async.resolve();
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
