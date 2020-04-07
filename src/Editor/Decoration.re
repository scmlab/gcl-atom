open Belt;

let mark = (type_, class_, loc, editor) => {
  open Atom;
  let range = Base2.Loc.toRange(loc);
  let marker = TextEditor.markBufferRange(range, editor);
  let option = TextEditor.decorateMarkerOptions(~type_, ~class_, ());
  Atom.TextEditor.decorateMarker(marker, option, editor);
};

let markLineSpecSoft = mark("highlight", "highlight-spec-soft");
let markLineSpecHard = mark("highlight", "highlight-spec-hard");

//

let overlay =
    (text, class_, tail: bool, translation: (int, int), loc, editor) => {
  open Atom;
  open Webapi.Dom;
  let range = Base2.Loc.toRange(loc);

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

let overlayError = (loc, editor) => {
  let length =
    editor
    |> Atom.TextEditor.getTextInBufferRange(Base2.Loc.toRange(loc))
    |> Js.String.length;
  let text = Js.String.repeat(length, "&nbsp;");
  overlay(text, "overlay-error", true, (0, 0), loc, editor);
};

let markSpec =
    (editor, spec: Guacamole.GCL.Response.Specification.t)
    : array(Atom.Decoration.t) => {
  Guacamole.GCL.Response.Specification.(
    switch (spec.loc) {
    | NoLoc => [||]
    | Loc(start, end_) =>
      open Guacamole.GCL;
      let startLoc = Loc.Loc(start, Pos.translateBy(0, 2, start));
      let endLoc = Loc.Loc(Pos.translateBy(0, -2, end_), end_);

      let trim = s =>
        if (String.length(s) > 77) {
          String.sub(s, 0, 73) ++ " ...";
        } else {
          s;
        };

      let pre = trim(Syntax.Pred.toString(spec.pre));
      let post = trim(Syntax.Pred.toString(spec.post));

      // see if the Spec's precondition and the post-condition look the same (i.e. the Q_Q case)
      let isQQ = pre == post;

      Js.List.flatten([
        overlaySpec(isQQ ? "" : pre, startLoc, editor),
        overlaySpec(isQQ ? "" : post, endLoc, editor),
        [
          markLineSpecSoft(startLoc, editor),
          markLineSpecSoft(endLoc, editor),
        ],
      ])
      |> List.toArray;
    }
  );
};

let markSite = (site, specifications, editor) => {
  let loc = specifications |> Guacamole.GCL.Response.Error.Site.toLoc(site);
  Js.List.flatten([
    overlayError(loc, editor),
    [mark("line-number", "line-number-error", loc, editor)],
  ])
  |> List.toArray;
};

let markLink = mark("highlight", "highlight-link");