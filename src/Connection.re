open! Rebase;
module Conn = AgdaMode.Connection2;

module Error = Conn.Error;

type t = {
  mutable path: string,
  mutable process: Conn.Process.t,
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
      | Error(e) =>
        connection.emitter.emit(Error(Error.ConnectionError(e))),
    );

  ();
};
let make = (): Promise.t(result(t, Error.t)) => {
  Conn.PathSearch.run("gcl")
  ->Promise.map(
      fun
      | Error(e) => Error(Conn.Error.PathSearchError(e))
      | Ok(v) => Ok(v),
    )
  ->Promise.mapOk(path => {
      let process = Conn.Process.make(path, [||]);
      {path, process, emitter: Event.make()};
    })
  ->Promise.tapOk(connection => wire(connection));
};

let send = (request, connection): Promise.t(result(Js.Json.t, Error.t)) => {
  let promise = connection.emitter.once();
  let result = connection.process.send(request);
  switch (result) {
  | Ok () => promise
  | Error(e) => Promise.resolved(Error(Error.ConnectionError(e)))
  };
};
