open Atom;
//
// View
//
module View = (Editor: Guacamole.Sig.Editor) => {
  let make = (_, editor) => View.make2(editor);
  let destroy = view => View.destroy2(view);
};

module rec Impl:
  Guacamole.Sig.Editor with
    type editor = TextEditor.t and
    type context = CompositeDisposable.t and
    type disposable = Disposable.t = {
  open Belt;

  type editor = TextEditor.t;
  type context = CompositeDisposable.t;
  type disposable = Disposable.t;
  type view = Types.View.Interface.t;
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
    Workspace.onDidChangeActiveTextEditor(next =>
      if (next != previous^) {
        callback(
          (previous^)->Option.flatMap(TextEditor.getPath),
          next->Option.flatMap(TextEditor.getPath),
        );
        previous := next;
      }
    );
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

  module View = View(Impl);
};
