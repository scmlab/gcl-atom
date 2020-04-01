open Belt;

module Main = Guacamole.Main.Impl(AtomImpl);

// semaphore with Atom.CompositeDisposable.t
let activated: ref(option(Atom.CompositeDisposable.t)) = ref(None);

// the entry point of the whole package, may be invoked several times
let activate = _ => {
  // to prevent the aforementioned, only activate when the semaphore is None
  if ((activated^)->Option.isNone) {
    let subscriptions = Atom.CompositeDisposable.make();
    activated := Some(subscriptions);
    Main.make(subscriptions);
  };
  Js.Promise.resolve();
};

// only deactivate when the semaphore is Some(subscriptions)
let deactivate = _ =>
  (activated^)
  ->Option.forEach(subscriptions => {
      Atom.CompositeDisposable.dispose(subscriptions);
      activated := None;
      Main.destroy();
    });

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