open Atom;
open! Belt;

module Impl:
  Guacamole.Sig.Editor with
    type editor = TextEditor.t and type context = CompositeDisposable.t = {
  type editor = TextEditor.t;
  type context = CompositeDisposable.t;
  module Disposable = {
    type t = Disposable.t;
    let make = Disposable.make;
    let dispose = Disposable.dispose;
  };
  type view = View.t;

  module Point = {
    type t = Point.t;
    let make = Point.make;
    let line = Point.row;
    let column = Point.column;
    let translate = (p, x, y) => Point.translate(Point.make(x, y), p);
    open Guacamole.GCL.Pos;
    let fromPos =
      fun
      | Pos(_, line, column) => make(line - 1, column - 1);
    let toPos = (filepath, point) => {
      Pos(filepath, line(point) + 1, column(point) + 1);
    };
  };

  module Range = {
    type t = Atom.Range.t;
    let make = Atom.Range.make;
    let start = Atom.Range.start;
    let end_ = Atom.Range.end_;
    open Guacamole.GCL.Loc;
    let fromLoc =
      fun
      | NoLoc => make(Point.make(0, 0), Point.make(0, 0))
      | Loc(x, Pos(_, line, column)) =>
        make(Point.fromPos(x), Point.make(line - 1, column));
    let toLoc = (filepath, range) => {
      let start = start(range);
      let end_ = end_(range);
      Loc(
        Pos(filepath, Point.line(start) + 1, Point.column(start) + 1),
        Pos(filepath, Point.line(end_) + 1, Point.column(end_)),
      );
    };
  };

  type decoration = Atom.Decoration.t;
  type fileName = string;

  // // if it ends with '.gcl'
  // let isGCLFile = (editor): bool => {
  //   let filepath =
  //     editor->TextEditor.getPath->Option.getWithDefault("untitled");
  //   Js.Re.test_([%re "/\\.gcl$/i"], filepath);
  // };

  let editorType = Guacamole.Sig.Atom;

  let getExtensionPath = _ =>
    Packages.resolvePackagePath("gcl-atom")->Option.getWithDefault("");

  let getFileName = editor => TextEditor.getPath(editor);

  let save = editor =>
    editor
    ->TextEditor.save
    ->Promise.Js.fromBsPromise
    ->Promise.Js.toResult
    ->Promise.map(
        fun
        | Error(_) => false
        | Ok(_) => true,
      );

  let addToSubscriptions = (disposable, subscriptions) =>
    disposable |> CompositeDisposable.add(subscriptions);

  // when the editor got closed
  let onDidCloseEditor = callback =>
    Workspace.observeTextEditors(editor => {
      let subscriptions = CompositeDisposable.make();
      editor
      |> TextEditor.onDidDestroy(() => {
           TextEditor.getPath(editor)->Option.forEach(callback);
           CompositeDisposable.dispose(subscriptions);
         })
      |> CompositeDisposable.add(subscriptions);
    });
  // Workspace.onDidChangeActiveTextEditor(next => {});

  let onDidChangeFileName = callback =>
    Workspace.observeTextEditors(editor => {
      let subscriptions = CompositeDisposable.make();
      let previous = ref(TextEditor.getPath(editor));
      editor
      |> TextEditor.onDidChangePath(() => {
           let next = TextEditor.getPath(editor);
           if (next != previous^) {
             callback(previous^, next);
             previous := next;
           };
         })
      |> CompositeDisposable.add(subscriptions);
    });

  let onDidChangeActivation = callback => {
    let previous = ref(Workspace.getActiveTextEditor());
    Workspace.onDidChangeActiveTextEditor(next => {
      let previousFileName = (previous^)->Option.flatMap(TextEditor.getPath);
      let nextFileName = next->Option.flatMap(TextEditor.getPath);
      if (previousFileName != nextFileName) {
        callback(previousFileName, nextFileName);
        previous := next;
      };
    });
  };

  let registerCommand = (name, callback) => {
    // find the <TextEditor> targeted by the given event
    let eventTargetEditor = (event: Webapi.Dom.Event.t): option(TextEditor.t) => {
      // the HtmlElement of the event target
      let targetSubElement =
        event
        |> Webapi.Dom.Event.target
        |> Webapi.Dom.EventTarget.unsafeAsElement
        |> Webapi.Dom.Element.unsafeAsHtmlElement;

      // the <TextEditor>s that contain the event target
      let targetedEditors =
        Workspace.getTextEditors()
        ->Array.keep(x =>
            x
            |> Views.getView
            |> Webapi.Dom.HtmlElement.asNode
            |> Webapi.Dom.Node.contains(targetSubElement)
          );

      targetedEditors[0];
    };

    Commands.add(`CSSSelector("atom-text-editor"), "gcl-atom:" ++ name, event => {
      eventTargetEditor(event)->Option.forEach(callback)
    });
  };

  // let getActiveEditor = () => Window.activeTextEditor;

  module Config = {
    let setGCLPath = value => {
      Config.set("gcl-atom.path", value) |> ignore;
      Promise.resolved();
    };
    let getGCLPath = () => Config.get("gcl-atom.path");
  };

  module View = View;

  module Decoration = {
    type kind =
      | Error
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

    let highlightBackground = (editor: editor, kind: kind, range: Range.t) => {
      let createMarker = (class_, range) => {
        let marker = TextEditor.markBufferRange(range, editor);
        let option =
          TextEditor.decorateMarkerOptions(~type_="highlight", ~class_, ());
        Atom.TextEditor.decorateMarker(marker, option, editor);
      };
      switch (kind) {
      | Error => [|createMarker("highlight-error", range)|]
      | Spec => [|createMarker("highlight-spec", range)|]
      };
    };

    let overlayText =
        (editor: editor, kind: kind, text: string, range: Range.t) => {
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
      | Error => [|
          createOverlay(text, "overlay-error", true, (0, 0), range),
        |]
      | Spec => [|
          createOverlay(text, "overlay-spec", false, (0, 1), range),
        |]
      };
    };

    let destroy = Atom.Decoration.destroy;
  };
};