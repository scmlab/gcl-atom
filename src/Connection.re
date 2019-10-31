open Rebase;

module Error = {
  type autoSearch =
    | ProcessHanging(string)
    | NotSupported(string)
    | NotFound(string, string);

  type validation =
    /* the path is empty */
    | PathMalformed(string)
    /* the process is not responding */
    | ProcessHanging
    /* from the shell */
    | NotFound(Js.Exn.t)
    | ShellError(Js.Exn.t)
    /* from its stderr */
    | ProcessError(string)
    /* the process is not GCL */
    | IsNotGCL(string);

  type connection =
    | ShellError(Js.Exn.t)
    | ClosedByProcess(int, string)
    | DisconnectedByUser
    | NotEstablishedYet;

  type t =
    | AutoSearchError(autoSearch)
    | ValidationError(string, validation)
    | ConnectionError(connection);

  let toString =
    fun
    | AutoSearchError(ProcessHanging("gcl")) => (
        {js|Process not responding|js},
        {j|Please restart the process|j},
      )
    | AutoSearchError(ProcessHanging(name)) => (
        "Process not responding when looking for \"" ++ name ++ "\"",
        {j|Please restart the process|j},
      )
    | AutoSearchError(NotSupported(os)) => (
        "Auto search failed",
        {j|currently auto path searching is not supported on $(os)|j},
      )
    | AutoSearchError(NotFound("gcl", msg)) => ("Auto search failed", msg)
    | AutoSearchError(NotFound(name, msg)) => (
        "Auto search failed when looking for \"" ++ name ++ "\"",
        msg,
      )
    | ValidationError(_path, PathMalformed(msg)) => ("Path malformed", msg)
    | ValidationError(_path, ProcessHanging) => (
        "Process hanging",
        "The program has not been responding for more than 1 sec",
      )
    | ValidationError(_path, NotFound(error)) => (
        "GCL not found",
        Util.JsError.toString(error),
      )
    | ValidationError(_path, ShellError(error)) => (
        "Error from the shell",
        Util.JsError.toString(error),
      )
    | ValidationError(_path, ProcessError(msg)) => (
        "Error from the stderr",
        msg,
      )
    | ValidationError(_path, IsNotGCL(msg)) => ("This is not GCL", msg)
    | ConnectionError(ShellError(error)) => (
        "Socket error",
        Util.JsError.toString(error),
      )
    | ConnectionError(ClosedByProcess(code, signal)) => (
        "Socket closed by GCL",
        {j|code: $code
signal: $signal
|j},
      )
    | ConnectionError(DisconnectedByUser) => (
        "Disconnected",
        "Connection disconnected by ourselves",
      )
    | ConnectionError(NotEstablishedYet) => (
        "Connection not established yet",
        "Please establish the connection first",
      );
};

type t = {
  mutable path: option(string),
  mutable process: option(N.ChildProcess.t),
  emitter: Event.t(Js.Json.t, Error.t),
};

let disconnect = connection => {
  connection.emitter |> Event.destroy;
  connection.process |> Option.forEach(N.ChildProcess.kill("SIGTERM"));
  connection.process = None;
};

let isConnected = connection =>
  switch (connection.process) {
  | None => false
  | Some(_) => true
  };

let make = (): t => {path: None, process: None, emitter: Event.make()};

let autoSearch = (name): Async.t(string, Error.t) =>
  Async.make((resolve, reject) => {
    /* reject if the process hasn't responded for more than 1 second */
    let hangTimeout =
      Js.Global.setTimeout(
        () => reject(ProcessHanging(name): Error.autoSearch),
        1000,
      );
    let commandName =
      switch (N.OS.type_()) {
      | "Linux"
      | "Darwin" => Ok("which")
      | "Windows_NT" => Ok("where.exe")
      | os => Error(os)
      };

    switch (commandName) {
    | Error(os) => reject(NotSupported(os))
    | Ok(commandName') =>
      N.ChildProcess.exec(
        commandName' ++ " " ++ name,
        (error, stdout, stderr) => {
          /* clear timeout as the process has responded */
          Js.Global.clearTimeout(hangTimeout);

          /* error */
          switch (error |> Js.Nullable.toOption) {
          | None => ()
          | Some(err) =>
            reject(
              NotFound(name, err |> Js.Exn.message |> Option.getOr("")),
            )
          };

          /* stderr */
          let stderr' = stderr |> Node.Buffer.toString;
          if (stderr' |> String.isEmpty |> (!)) {
            reject(NotFound(name, stderr'));
          };

          /* stdout */
          let stdout' = stdout |> Node.Buffer.toString;
          if (stdout' |> String.isEmpty) {
            reject(NotFound(name, ""));
          } else {
            resolve(stdout');
          };
        },
      )
      |> ignore
    };
  })
  |> Async.mapError(e => Error.AutoSearchError(e));

let connect = connection =>
  autoSearch("gcl")
  |> Async.thenOk(path => {
       open N;
       let process = ChildProcess.spawn(path, [||], {"shell": true});
       connection.path = Some(path);
       connection.process = Some(process);

       // for incremental parsing
       let unfinishedMsg = ref(None);

       // on data
       process
       |> N.ChildProcess.stdout
       |> N.Stream.Readable.on(
            `data(
              chunk => {
                // serialize the binary chunk into string
                let string = Node.Buffer.toString(chunk);

                // for incremental parsing
                let augmented =
                  switch (unfinishedMsg^) {
                  | None => string
                  | Some(unfinished) => unfinished ++ string
                  };

                // try parsing the string as JSON value
                switch (Json.parse(augmented)) {
                | None => unfinishedMsg := Some(augmented)
                | Some(result) => connection.emitter |> Event.emitOk(result)
                };
              },
            ),
          )
       |> ignore;

       // on errors and anomalies
       process
       |> ChildProcess.on(
            `error(
              exn => {
                disconnect(connection) |> ignore;
                connection.emitter
                |> Event.emitError(
                     Error.ConnectionError(Error.ShellError(exn)),
                   );
              },
            ),
          )
       |> ChildProcess.on(
            `close(
              (code, signal) => {
                disconnect(connection) |> ignore;
                connection.emitter
                |> Event.emitError(
                     Error.ConnectionError(
                       Error.ClosedByProcess(code, signal),
                     ),
                   );
              },
            ),
          )
       |> ignore;

       Async.resolve();
     });

let send = (request, connection): Async.t(Js.Json.t, Error.t) => {
  switch (connection.process) {
  | None => Async.reject(Error.ConnectionError(Error.NotEstablishedYet))
  | Some(process) =>
    let promise = connection.emitter |> Event.once;

    let payload = Node.Buffer.fromString(request ++ "\n");
    // write
    process
    |> N.ChildProcess.stdin
    |> N.Stream.Writable.write(payload)
    |> ignore;

    promise;
  };
};
