open Belt;

module States = Guacamole.Main.StateDict.Impl(AtomImpl);
module State = Guacamole.State.Impl(AtomImpl);
module Editor = AtomImpl;

let make = subscriptions => {
  let isGCL = Js.Re.test_([%re "/\\.gcl$/i"]);

  // when a TextEditor gets closed, destroy the corresponding State
  Editor.onDidCloseEditor(States.destroy)
  ->Editor.addToSubscriptions(subscriptions);
  // when a file got renamed, destroy the corresponding State if it becomes non-GCL
  Editor.onDidChangeFileName((oldName, newName) =>
    oldName->Option.forEach(oldName =>
      newName->Option.forEach(newName =>
        if (States.contains(oldName)) {
          if (isGCL(newName)) {
            States.rename(oldName, newName);
          } else {
            States.destroy(oldName);
          };
        }
      )
    )
  )
  ->Editor.addToSubscriptions(subscriptions);
  // on editor activation, reveal the corresponding Panel (if any)
  Editor.onDidChangeActivation((_previous, next) => {
    next->Option.flatMap(States.get)->Option.forEach(Js.log2("[activate]"))
  })
  ->Editor.addToSubscriptions(subscriptions);

  // on load
  Editor.registerCommand("load", editor => {
    let fileName = editor->Editor.editorFileName;
    if (isGCL(fileName)) {
      // see if it's already in the States
      switch (States.get(fileName)) {
      | None =>
        Js.log("[ main ][ first LOAD ]");
        let state = State.make(subscriptions, editor);
        States.add(fileName, state);
      | Some(_state) => Js.log("[ main ][ LOAD ]")
      };
    };
  })
  ->Editor.addToSubscriptions(subscriptions);
};
let destroy = () => {
  States.destroyAll();
};

let activated: ref(bool) = ref(false);

// the entry point of the whole package, may be invoked several times
let activate = _ => {
  if (! activated^) {
    activated := true;
    let subscriptions = Atom.CompositeDisposable.make();
    make(subscriptions);
  };
  Js.Promise.resolve();
};

let deactivate = _ =>
  if (activated^) {
    // Instances.destroyAll();
    // CompositeDisposable.dispose(subscriptions);
    activated := false;
    destroy();
  };

// https://atom.io/docs/api/latest/Config
let config = {
  "path": {
    "title": "GCL path",
    "description": "Path to the executable of GCL, automatically inferred when possible. Overwrite to override.",
    "type": "string",
    "default": "",
    "order": 1,
  },
};