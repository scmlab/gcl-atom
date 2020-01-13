open Rebase;

module Site = {
  type t =
    | Global(Atom.Range.t)
    | Local(Atom.Range.t, int);

  open Json.Decode;
  open Decoder;

  let decode: decoder(t) =
    sum(
      fun
      | "Global" => Contents(json => Global(json |> range))
      | "Local" =>
        Contents(pair(range, int) |> map(((r, i)) => Local(r, i)))
      | tag => raise(DecodeError("Unknown constructor: " ++ tag)),
    );

  let toRange = (site, specifications) => {
    switch (site) {
    | Global(range) => range
    | Local(range, i) =>
      open Atom.Range;
      open Specification;
      let specs = specifications |> Array.filter(spec => spec.id == i);

      specs[0]
      |> Option.mapOr(
           spec =>
             range
             |> translate(start(spec.range), start(spec.range))
             // down by 1 line
             |> translate(Atom.Point.make(1, 0), Atom.Point.make(1, 0)),
           range,
         );
    };
  };

  let toString = site => {
    let rangeToString = range => {
      Atom.Range.(
        Atom.Point.(
          string_of_int(row(start(range)))
          ++ ":"
          ++ string_of_int(column(start(range)))
          ++ "-"
          ++ string_of_int(row(end_(range)))
          ++ ":"
          ++ string_of_int(column(end_(range)))
        )
      );
    };
    switch (site) {
    | Global(range) => "at " ++ rangeToString(range)
    | Local(range, i) =>
      "at " ++ rangeToString(range) ++ " in #" ++ string_of_int(i)
    };
  };
};

let mark = (type_, class_, range, editor) => {
  open Atom;
  let marker = editor |> TextEditor.markBufferRange(range);
  let option = TextEditor.decorateMarkerOptions(~type_, ~class_, ());
  [editor |> Atom.TextEditor.decorateMarker(marker, option)];
};

let markLineSpecSoft = mark("highlight", "highlight-spec-soft");
let markLineSpecHard = mark("highlight", "highlight-spec-hard");

//

let overlay =
    (
      text,
      class_,
      tail: bool,
      translation: (int, int),
      range: Atom.Range.t,
      editor,
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
  let marker = editor |> TextEditor.markBufferRange(range);
  let option =
    TextEditor.decorateMarkerOptions(
      ~type_="overlay",
      ~position=tail ? "tail" : "head",
      ~item=Element.unsafeAsHtmlElement(element),
      (),
    );
  [editor |> TextEditor.decorateMarker(marker, option)];
};

let overlaySpec = text => overlay(text, "overlay-spec-text", false, (0, 1));

let overlayError = (range: Atom.Range.t, editor) => {
  let length =
    editor |> Atom.TextEditor.getTextInBufferRange(range) |> Js.String.length;
  let text = Js.String.repeat(length, "&nbsp;");
  overlay(text, "overlay-error", true, (0, 0), range, editor);
};

let markSpec = (spec: Specification.t, editor): array(Atom.Decoration.t) => {
  open Specification;
  open Atom;

  let start = Range.start(spec.range);
  let start = Range.make(start, Point.translate(Point.make(0, 2), start));
  let end_ = Range.end_(spec.range);
  let end_ = Range.make(Point.translate(Point.make(0, -2), end_), end_);

  let trim = s =>
    if (String.length(s) > 77) {
      String.sub(~from=0, ~length=73, s) ++ " ...";
    } else {
      s;
    };

  let pre = trim(Expr.toString(spec.pre));
  let post = trim(Expr.toString(spec.post));

  Js.List.flatten([
    overlaySpec(pre, start, editor),
    overlaySpec(post, end_, editor),
    markLineSpecSoft(end_, editor),
  ])
  |> Array.fromList;
  // Js.List.flatten([
  //   switch (spec.hardness) {
  //   | Hard => [markLineSpecHard(start, editor)]
  //   | Soft => [markLineSpecSoft(start, editor)]
  //   },
  //   overlaySpec(pre, start, editor),
  //   overlaySpec(post, end_, editor),
  //   markLineSpecSoft(end_, editor),
  // ]);
};

let markSite = (site, specifications, editor) => {
  let range = specifications |> Site.toRange(site);
  Js.List.flatten([
    overlayError(range, editor),
    mark("line-number", "line-number-error", range, editor),
  ])
  |> Array.fromList;
};

// // destroy all decorations
// let destroyAll = decorations =>
//   instance.decorations |> Array.forEach(Atom.Decoration.destroy);
