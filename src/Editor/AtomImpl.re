open Atom;
open! Belt;

module rec Impl:
  Guacamole.Sig.Editor with
    type editor = TextEditor.t and
    type context = CompositeDisposable.t and
    type disposable = Disposable.t = {
  type editor = TextEditor.t;
  type context = CompositeDisposable.t;
  type disposable = Disposable.t;
  type view = Types.View.t;
  type point = Point.t;
  type range = Atom.Range.t;
  type fileName = string;

  // // if it ends with '.gcl'
  // let isGCLFile = (editor): bool => {
  //   let filepath =
  //     editor->TextEditor.getPath->Option.getWithDefault("untitled");
  //   Js.Re.test_([%re "/\\.gcl$/i"], filepath);
  // };

  let getExtensionPath = _ =>
    Packages.resolvePackagePath("gcl-atom")->Option.getWithDefault("");

  let getFileName = editor =>
    TextEditor.getPath(editor)->Option.getWithDefault("");

  let toPoint =
    fun
    | Guacamole.GCL.Pos.Pos(_, line, column) =>
      Atom.Point.make(line - 1, column - 1);
  let fromPoint = (filepath, point) => {
    Guacamole.GCL.Pos.Pos(
      filepath,
      Atom.Point.row(point) + 1,
      Atom.Point.column(point) + 1,
    );
  };

  let toRange =
    fun
    | Guacamole.GCL.Loc.NoLoc =>
      Atom.Range.make(Atom.Point.make(0, 0), Atom.Point.make(0, 0))
    | Loc(x, Pos(_, line, column)) =>
      Atom.Range.make(toPoint(x), Atom.Point.make(line - 1, column));
  let fromRange = (filepath, range) => {
    let start = Atom.Range.start(range);
    let end_ = Atom.Range.end_(range);
    Guacamole.GCL.Loc.Loc(
      Pos(
        filepath,
        Atom.Point.row(start) + 1,
        Atom.Point.column(start) + 1,
      ),
      Pos(filepath, Atom.Point.row(end_) + 1, Atom.Point.column(end_)),
    );
  };

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

  module View = View.Impl(Impl);
};