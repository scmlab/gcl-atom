open! Rebase;

module Process = AgdaMode.Process;

module Error = {
  type t =
    | PathSearch(Process.PathSearch.Error.t)
    | Validation(Process.Validation.Error.t)
    | Process(Process.Error.t);

  let toString =
    fun
    | PathSearch(e) => Process.PathSearch.Error.toString(e)
    | Validation(e) => Process.Validation.Error.toString(e)
    | Process(e) => Process.Error.toString(e);
};

type t = {
  mutable path: string,
  mutable process: Process.t,
  mutable emitter: Event.t(result(Js.Json.t, Error.t)),
};

let isConnected = connection => connection.process.isConnected();
let disconnect = connection => connection.process.disconnect();

// Wires `process` and `emitter` up
let wire = connection => {
  // for incremental parsing
  let unfinishedMsg = ref(None);

  // on data
  let _destructor =
    connection.process.emitter.on(
      fun
      | Ok(data) => {
          // for incremental parsing
          let augmented =
            switch (unfinishedMsg^) {
            | None => data
            | Some(unfinished) => unfinished ++ data
            };

          // try parsing the string as JSON value
          switch (Json.parse(augmented)) {
          | None => unfinishedMsg := Some(augmented)
          | Some(result) =>
            unfinishedMsg := None;
            connection.emitter.emit(Ok(result)) |> ignore;
          };
        }
      | Error(e) => connection.emitter.emit(Error(Error.Process(e))),
    );

  ();
};

let getGCLPath = (): Promise.t(result(string, Error.t)) => {
  let storedPath =
    Atom.Config.get("gcl-atom.path") |> Option.getOr("") |> Js.String.trim;
  if (String.isEmpty(storedPath) || storedPath == ".") {
    Process.PathSearch.run("gcl")
    ->Promise.mapOk(Js.String.trim)
    ->Promise.mapError(e => Error.PathSearch(e));
  } else {
    Promise.resolved(Ok(storedPath));
  };
};

let validateGCLPath = (path): Promise.t(result(string, Error.t)) =>
  Process.Validation.run(path ++ " --help", output =>
    if (Js.Re.test_([%re "/^GCL/"], output)) {
      Ok(path);
    } else {
      Error(path);
    }
  )
  ->Promise.mapError(e => Error.Validation(e));

let make = (): Promise.t(result(t, Error.t)) => {
  getGCLPath()
  ->Promise.flatMapOk(validateGCLPath)
  ->Promise.tapOk(path => Atom.Config.set("gcl-atom.path", path) |> ignore)
  ->Promise.mapOk(path => {
      let process = Process.make(path, [||]);
      {path, process, emitter: Event.make()};
    })
  ->Promise.tapOk(wire);
};

let send = (request, connection): Promise.t(result(Js.Json.t, Error.t)) => {
  let promise = connection.emitter.once();
  let result = connection.process.send(request);
  switch (result) {
  | Ok () => promise
  | Error(e) => Promise.resolved(Error(Error.Process(e)))
  };
};
