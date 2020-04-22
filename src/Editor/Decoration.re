open Atom;

type t = Atom.Decoration.t;

type kind =
  | Error
  | Highlight
  | Spec;

// rewrite "?" to "{!!}"
let digHole = (editor, range) => {
  let start = Atom.Range.start(range);
  // add indentation to the hole
  let indent = Js.String.repeat(Atom.Point.column(start), " ");
  let holeText = "{!\n" ++ indent ++ "\n" ++ indent ++ "!}";
  let holeRange =
    Atom.Range.make(
      start,
      Atom.Point.translate(start, Atom.Point.make(0, 1)),
    );
  editor
  |> Atom.TextEditor.setTextInBufferRange(holeRange, holeText)
  |> ignore;
  // set the cursor inside the hole
  let cursorPos = Atom.Point.translate(start, Atom.Point.make(1, 0));
  editor |> Atom.TextEditor.setCursorBufferPosition(cursorPos);
};

let highlightBackground = (editor, kind: kind, range) => {
  let createMarker = (class_, range) => {
    let marker = TextEditor.markBufferRange(range, editor);
    let option =
      TextEditor.decorateMarkerOptions(~type_="highlight", ~class_, ());
    Atom.TextEditor.decorateMarker(marker, option, editor);
  };
  switch (kind) {
  | Error => [|createMarker("highlight-error", range)|]
  | Highlight => [|createMarker("highlight-link", range)|]
  | Spec => [|createMarker("highlight-spec", range)|]
  };
};

let overlayText = (editor, kind: kind, text: string, range: Range.t) => {
  let createOverlay =
      (text, class_, tail: bool, translation: (int, int), range) => {
    open Webapi.Dom;

    // create an element for the overlay
    let element = document |> Document.createElement("div");
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
    let marker = editor |> TextEditor.markBufferRange(range);
    let option =
      TextEditor.decorateMarkerOptions(
        ~type_="overlay",
        ~position=tail ? "tail" : "head",
        ~item=Element.unsafeAsHtmlElement(element),
        (),
      );
    TextEditor.decorateMarker(marker, option, editor);
  };

  switch (kind) {
  | Error => [|createOverlay(text, "overlay-error", true, (0, 0), range)|]
  | Highlight => [|
      createOverlay(text, "overlay-link", false, (0, 1), range),
    |]
  | Spec => [|createOverlay(text, "overlay-spec", false, (0, 1), range)|]
  };
};

let destroy = Atom.Decoration.destroy;