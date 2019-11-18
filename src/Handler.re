open Rebase;

open Response;

let markLineError = (range, instance: Type.instance) => {
  open Atom;
  let marker = instance.editor |> TextEditor.markBufferRange(range);
  let option =
    TextEditor.decorateMarkerOptions(
      ~type_="line",
      ~class_="marker-error",
      (),
    );
  let decoration =
    instance.editor |> Atom.TextEditor.decorateMarker(marker, option);

  instance.decorations = Array.concat(instance.decorations, [|decoration|]);
};

let markRangeError = (range, instance: Type.instance) => {
  open Atom;
  let marker = instance.editor |> TextEditor.markBufferRange(range);
  let option =
    TextEditor.decorateMarkerOptions(
      ~type_="highlight",
      ~class_="marker-error",
      (),
    );
  let decoration =
    instance.editor |> Atom.TextEditor.decorateMarker(marker, option);

  instance.decorations = Array.concat(instance.decorations, [|decoration|]);
};

let digHole = (range, instance: Type.instance) => {
  open Atom;
  // rewrite "?" to "{!!}"
  let start = Range.start(range);
  let range' = Range.make(start, Point.translate(start, Point.make(0, 1)));
  instance.editor
  |> TextEditor.setTextInBufferRange(range', "{!\n\n!}")
  |> ignore;

  Async.resolve();
  // // mark the lines
  // let marker = instance.editor |> TextEditor.markBufferRange(resultRange);
  // let option =
  //   TextEditor.decorateMarkerOptions(
  //     ~type_="highlight",
  //     ~class_="marker-error",
  //     (),
  //   );
  // let decoration =
  //   instance.editor |> Atom.TextEditor.decorateMarker(marker, option);
  //
  // instance.decorations = Array.concat(instance.decorations, [|decoration|]);
  // //
  // // |> Js.String.replaceByRe(
  // //      [%re "/\\{!.*!\\}/"],
  // //      "{!" ++ content ++ padding ++ "!}",
  // //    );
};
