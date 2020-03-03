open Rebase;
open Base;
open Types.Instance;
open GCL.Response.Specification;

let fromCursorPosition = instance => {
  open Atom;

  let cursor = instance.editor |> Atom.TextEditor.getCursorBufferPosition;
  // find the smallest hole containing the cursor
  let smallestHole = ref(None);
  instance.specifications
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

let getPayloadRange = (spec, instance) => {
  open Atom;

  let startingRow = Point.row(Range.start(Loc.toRange(spec.loc))) + 1;
  let endingRow = Point.row(Range.end_(Loc.toRange(spec.loc))) - 1;

  let start =
    instance.editor
    |> TextEditor.getBuffer
    |> TextBuffer.rangeForRow(startingRow, true)
    |> Range.start;
  let end_ =
    instance.editor
    |> TextEditor.getBuffer
    |> TextBuffer.rangeForRow(endingRow, true)
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
       Js.log2("!!!! [ payload ]", payload);
       let start = Range.start(Loc.toRange(spec.loc));

       instance.editor
       |> TextEditor.getBuffer
       |> TextBuffer.delete(Loc.toRange(spec.loc))
       |> ignore;

       instance.editor
       |> TextEditor.getBuffer
       |> TextBuffer.insert(start, Js.String.trim(payload))
       |> ignore;
     });
  Promise.resolved();
};

// NOTE: move this somewhere else
module Site = {
  open GCL.Error.Site;
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
let digHole = (site, instance) => {
  let range = instance.specifications |> Site.toRange(site);
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
