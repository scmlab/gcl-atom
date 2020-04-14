// open Belt;

// // open Guacamole.Main.States;

// let activated: ref(bool) = ref(false);

// let states: Js.Dict.t(State.t) = Js.Dict.empty();

// module Instances = {
//   let textEditorID = textEditor =>
//     string_of_int(Atom.TextEditor.id(textEditor));
//   // let textEditorID = Atom.TextEditor.id >> string_of_int;

//   let get = textEditor => {
//     Js.Dict.get(states, textEditorID(textEditor));
//   };
//   let getThen = (f, textEditor) => textEditor->get->Option.forEach(f);

//   let add = textEditor => {
//     switch (get(textEditor)) {
//     | Some(_) => ()
//     | None =>
//       State.make(textEditor)
//       |> Js.Dict.set(states, textEditorID(textEditor))
//     };
//   };

//   let delete_: string => unit = [%raw "function (id) {delete states[id]}"];
//   // destroy a certain Instance and remove it from `states`
//   let remove = textEditor => {
//     let id = textEditorID(textEditor);
//     switch (Js.Dict.get(states, id)) {
//     | Some(state) =>
//       State.destroy(state);
//       delete_(id) |> ignore;
//     | None => ()
//     };
//   };
//   // destroy all State in `states` and empty it
//   let destroyAll = () => {
//     states
//     ->Js.Dict.entries
//     ->Array.forEach(((id, states)) => {
//         State.destroy(states);
//         delete_(id) |> ignore;
//       });
//   };
//   let contains = textEditor => {
//     switch (get(textEditor)) {
//     | Some(_) => true
//     | None => false
//     };
//   };
//   let size = () => {
//     states |> Js.Dict.keys |> Array.length;
//   };
// };

// // if it ends with '.gcl'
// let isGCLFile = (textEditor): bool => {
//   let filepath =
//     textEditor->Atom.TextEditor.getPath->Option.getWithDefault("untitled");
//   Js.Re.test_([%re "/\\.gcl$/i"], filepath);
// };

// open Atom;

// let subscriptions = CompositeDisposable.make();

// /* textEditor active/inactive event */
// let onEditorActivationChange = () => {
//   let previous = ref(Workspace.getActiveTextEditor());
//   Workspace.onDidChangeActiveTextEditor(next => {
//     /* decativate the previously activated editor */
//     (previous^)->Option.forEach(Instances.getThen(State.hideView));
//     /* activate the next editor */
//     switch (next) {
//     | None => ()
//     | Some(nextEditor) =>
//       nextEditor |> Instances.getThen(State.showView);
//       previous := Some(nextEditor);
//     };
//   })
//   |> CompositeDisposable.add(subscriptions);
// };

// // find the <TextEditor> targeted by the given event
// let eventTargetEditor = (event: Webapi.Dom.Event.t): option(TextEditor.t) => {
//   // the HtmlElement of the event target
//   let targetSubElement =
//     event
//     |> Webapi.Dom.Event.target
//     |> Webapi.Dom.EventTarget.unsafeAsElement
//     |> Webapi.Dom.Element.unsafeAsHtmlElement;

//   // the <TextEditor>s that contain the event target
//   let targetedEditors =
//     Workspace.getTextEditors()
//     ->Array.keep(x =>
//         x
//         |> Views.getView
//         |> Webapi.Dom.HtmlElement.asNode
//         |> Webapi.Dom.Node.contains(targetSubElement)
//       );

//   targetedEditors[0];
// };

// /* register keymap bindings and emit commands */
// let onTriggerCommand = () => {
//   Types.Command.names->Array.forEach(command =>
//     Commands.add(
//       `CSSSelector("atom-text-editor"), "gcl-atom:" ++ command, event =>
//       event
//       ->eventTargetEditor
//       ->Option.flatMap(Instances.get)
//       ->Option.forEach(state =>
//           Task__Command.dispatch(Types.Command.parse(command))
//           |> TaskRunner.run(state)
//           |> ignore
//         )
//     )
//     |> CompositeDisposable.add(subscriptions)
//   );
// };

// // triggered everytime when a new text editor is opened
// let onOpenEditor = () => {
//   Workspace.observeTextEditors(textEditor => {
//     open CompositeDisposable;
//     let textEditorSubscriptions = make();

//     /* register it */
//     if (isGCLFile(textEditor)) {
//       Instances.add(textEditor);
//     };

//     /* subscribe to path change in case that `isAgdaFile(textEditor)` changed */
//     textEditor
//     |> TextEditor.onDidChangePath(() => {
//          /* agda => not agda */
//          if (!isGCLFile(textEditor) && Instances.contains(textEditor)) {
//            Instances.remove(textEditor);
//          };
//          /* not agda => agda */
//          if (isGCLFile(textEditor) && !Instances.contains(textEditor)) {
//            Instances.add(textEditor);
//          };
//        })
//     |> add(textEditorSubscriptions);

//     /* on destroy */
//     textEditor
//     |> TextEditor.onDidDestroy(() => {
//          if (isGCLFile(textEditor) && Instances.contains(textEditor)) {
//            Instances.remove(textEditor);
//          };
//          dispose(textEditorSubscriptions);
//        })
//     |> add(textEditorSubscriptions);
//   })
//   |> CompositeDisposable.add(subscriptions);
// };

// let setup = () => {
//   onOpenEditor();
//   onEditorActivationChange();
//   onTriggerCommand();
// };

// // the entry point of the whole package, should only be called once (before deactivation)
// let activate = _ => {
//   // make `activate` idempotent
//   if (! activated^) {
//     activated := true;
//     setup();
//   };
//   Js.Promise.resolve();
// };

// let deactivate = _ =>
//   if (activated^) {
//     activated := false;
//     Instances.destroyAll();
//     CompositeDisposable.dispose(subscriptions);
//   };

// // https://atom.io/docs/api/latest/Config
// let config = {
//   "path": {
//     "title": "GCL path",
//     "description": "Path to the executable of GCL, automatically inferred when possible. Overwrite to override.",
//     "type": "string",
//     "default": "",
//     "order": 1,
//   },
// };

();