open Belt;
open State;
open Guacamole.GCL.Response.Specification;

let fromCursorPosition = state => {
  let cursor = Atom.TextEditor.getCursorBufferPosition(state.editor);
  // find the smallest hole containing the cursor
  let smallestHole = ref(None);
  state.specifications
  ->Array.keep(spec =>
      Atom.Range.containsPoint(cursor, Base2.Loc.toRange(spec.loc))
    )
  ->Array.forEach(spec =>
      switch (smallestHole^) {
      | None => smallestHole := Some(spec)
      | Some(spec') =>
        if (Atom.Range.containsRange(
              Base2.Loc.toRange(spec.loc),
              Base2.Loc.toRange(spec'.loc),
            )) {
          smallestHole := Some(spec);
        }
      }
    );

  smallestHole^;
};

let getPayloadRange = (spec, state) => {
  let startingRow =
    Atom.Point.row(Atom.Range.start(Base2.Loc.toRange(spec.loc))) + 1;
  let endingRow =
    Atom.Point.row(Atom.Range.end_(Base2.Loc.toRange(spec.loc))) - 1;

  let start =
    state.editor
    |> Atom.TextEditor.getBuffer
    |> Atom.TextBuffer.rangeForRow(startingRow, true)
    |> Atom.Range.start;
  let end_ =
    state.editor
    |> Atom.TextEditor.getBuffer
    |> Atom.TextBuffer.rangeForRow(endingRow, true)
    |> Atom.Range.end_;
  Atom.Range.make(start, end_);
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
  let specs = state.specifications->Array.keep(spec => spec.id == i);
  specs[0]
  ->Option.forEach(spec => {
      let payload = getPayload(spec, state);
      Js.log2("!!!! [ payload ]", payload);
      let start = Atom.Range.start(Base2.Loc.toRange(spec.loc));

      state.editor
      |> Atom.TextEditor.getBuffer
      |> Atom.TextBuffer.delete(Base2.Loc.toRange(spec.loc))
      |> ignore;

      state.editor
      |> Atom.TextEditor.getBuffer
      |> Atom.TextBuffer.insert(start, Js.String.trim(payload))
      |> ignore;
    });
  Promise.resolved();
};

// NOTE: move this somewhere else
module Site = {
  open Guacamole.GCL.Response.Error.Site;
  let toLoc = (site, specifications) => {
    switch (site) {
    | Global(loc) => loc
    | Local(loc, i) =>
      let specs = specifications->Array.keep(spec => spec.id == i);

      specs[0]
      ->Option.mapWithDefault(loc, spec =>
          spec.loc
          |> Guacamole.GCL.Loc.translate(loc)
          |> Guacamole.GCL.Loc.translateBy(1, 0, 1, 0)
        );
    };
  };
  let toRange = (site, specifications) =>
    toLoc(site, specifications) |> Base2.Loc.toRange;
};

// rewrite "?" to "{!!}"
let digHole = (site, state) => {
  let range = Site.toRange(site, state.specifications);
  let start = Atom.Range.start(range);
  // add indentation to the hole
  let indent = Js.String.repeat(Atom.Point.column(start), " ");
  let holeText = "{!\n" ++ indent ++ "\n" ++ indent ++ "!}";
  let holeRange =
    Atom.Range.make(
      start,
      Atom.Point.translate(start, Atom.Point.make(0, 1)),
    );
  state.editor
  |> Atom.TextEditor.setTextInBufferRange(holeRange, holeText)
  |> ignore;
  // set the cursor inside the hole
  let cursorPos = Atom.Point.translate(start, Atom.Point.make(1, 0));
  state.editor |> Atom.TextEditor.setCursorBufferPosition(cursorPos);
  Promise.resolved();
};

let insert = (lineNo, expr, state) => {
  open Atom;

  let assertion = "{ " ++ Guacamole.GCL.Syntax.Expr.toString(expr) ++ " }\n";

  // set the cursor at the line
  let point = Point.make(lineNo - 1, 0);
  TextEditor.setCursorScreenPosition(point, state.editor);
  // insert the assertion
  TextEditor.insertText(assertion, state.editor) |> ignore;
};