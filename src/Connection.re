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
let make = (): Promise.t(result(t, Error.t)) => {
  Process.PathSearch.run("gcl")
  ->Promise.map(
      fun
      | Error(e) => Error(Error.PathSearch(e))
      | Ok(v) => Ok(v),
    )
  ->Promise.mapOk(path => {
      let process = Process.make(path, [||]);
      {path, process, emitter: Event.make()};
    })
  ->Promise.tapOk(connection => wire(connection));
};

let send = (request, connection): Promise.t(result(Js.Json.t, Error.t)) => {
  let promise = connection.emitter.once();
  let result = connection.process.send(request);
  switch (result) {
  | Ok () => promise
  | Error(e) => Promise.resolved(Error(Error.Process(e)))
  };
};
