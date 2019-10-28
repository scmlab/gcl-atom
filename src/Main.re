open Rebase;
// open Rebase.Fn;

let activated: ref(bool) = ref(false);

let instances: Js.Dict.t(Type.instance) = Js.Dict.empty();

module Instances = {
  let textEditorID = textEditor =>
    string_of_int(Atom.TextEditor.id(textEditor));
  // let textEditorID = Atom.TextEditor.id >> string_of_int;

  let get = textEditor => {
    Js.Dict.get(instances, textEditorID(textEditor));
  };
  let getThen = (f, textEditor) => textEditor |> get |> Option.forEach(f);

  let add = textEditor => {
    switch (get(textEditor)) {
    | Some(_instance) => ()
    | None =>
      Instance.make(textEditor)
      |> Js.Dict.set(instances, textEditorID(textEditor))
    };
  };

  let delete_: string => unit = [%raw id => "{delete instances[id]}"];
  // destroy a certain Instance and remove it from `instances`
  let remove = textEditor => {
    let id = textEditorID(textEditor);
    switch (Js.Dict.get(instances, id)) {
    | Some(instance) =>
      Instance.destroy(instance);
      delete_(id) |> ignore;
    | None => ()
    };
  };
  // destroy all Instance in `instances` and empty it
  let destroyAll = () => {
    instances
    |> Js.Dict.entries
    |> Array.forEach(((id, instance)) => {
         Instance.destroy(instance);
         delete_(id) |> ignore;
       });
  };
  let contains = textEditor => {
    switch (get(textEditor)) {
    | Some(_instance) => true
    | None => false
    };
  };
  let size = () => {
    instances |> Js.Dict.keys |> Array.length;
  };
};

// if it ends with '.gcl'
let isGCLFile = (textEditor): bool => {
  let filepath =
    textEditor |> Atom.TextEditor.getPath |> Option.getOr("untitled");
  Js.Re.test_([%re "/\\.gcl$/i"], filepath);
};

open Atom;

let subscriptions = CompositeDisposable.make();

/* textEditor active/inactive event */
let onEditorActivationChange = () => {
  let previous = ref(Workspace.getActiveTextEditor());
  Workspace.onDidChangeActiveTextEditor(next => {
    /* decativate the previously activated editor */
    previous^
    |> Option.forEach(
         Instances.getThen(x => x |> Instance.deactivate |> ignore),
       );
    /* activate the next editor */
    switch (next) {
    | None => ()
    | Some(nextEditor) =>
      nextEditor |> Instances.getThen(x => x |> Instance.activate |> ignore);
      previous := Some(nextEditor);
    };
  })
  |> CompositeDisposable.add(subscriptions);
};

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
    |> Array.filter(x =>
         x
         |> Views.getView
         |> Webapi.Dom.HtmlElement.asNode
         |> Webapi.Dom.Node.contains(targetSubElement)
       );

  targetedEditors[0];
};

/* register keymap bindings and emit commands */
let onTriggerCommand = () => {
  [|"activate", "save"|]
  |> Array.forEach(command =>
       Commands.add(
         `CSSSelector("atom-text-editor"), "gcl-atom:" ++ command, event =>
         event
         |> eventTargetEditor
         |> Option.flatMap(Instances.get)
         |> Option.forEach(instance =>
              instance |> Instance.dispatch(Request.parse(command)) |> ignore
            )
       )
       |> CompositeDisposable.add(subscriptions)
     );
};

// triggered everytime when a new text editor is opened
let onOpenEditor = () => {
  Workspace.observeTextEditors(textEditor => {
    open CompositeDisposable;
    let textEditorSubscriptions = make();

    /* register it */
    if (isGCLFile(textEditor)) {
      Instances.add(textEditor);
    };

    /* subscribe to path change in case that `isAgdaFile(textEditor)` changed */
    textEditor
    |> TextEditor.onDidChangePath(() => {
         /* agda => not agda */
         if (!isGCLFile(textEditor) && Instances.contains(textEditor)) {
           Instances.remove(textEditor);
         };
         /* not agda => agda */
         if (isGCLFile(textEditor) && !Instances.contains(textEditor)) {
           Instances.add(textEditor);
         };
       })
    |> add(textEditorSubscriptions);

    /* on destroy */
    textEditor
    |> TextEditor.onDidDestroy(() => {
         if (isGCLFile(textEditor) && Instances.contains(textEditor)) {
           Instances.remove(textEditor);
         };
         dispose(textEditorSubscriptions);
       })
    |> add(textEditorSubscriptions);
  })
  |> CompositeDisposable.add(subscriptions);
};

let setup = () => {
  onOpenEditor();
  onEditorActivationChange();
  onTriggerCommand();
};

// the entry point of the whole package, should only be called once (before deactivation)
let activate = _ => {
  // make `activate` idempotent
  if (! activated^) {
    activated := true;
    setup();
  };
  Js.Promise.resolve();
};

let deactivate = _ =>
  if (activated^) {
    activated := false;
    Instances.destroyAll();
    CompositeDisposable.dispose(subscriptions);
  };
