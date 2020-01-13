open! Rebase;

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
    | ClosedByProcess(int, string)
    | DisconnectedByUser
    | ShellError(Js.Exn.t)
    | ExitedByProcess(int, string)
    | NotEstablishedYet;

  type t =
    | DecodeError(string)
    | AutoSearchError(autoSearch)
    | ValidationError(string, validation)
    | ConnectionError(connection);

  let toString =
    fun
    | DecodeError(msg) => ({js|JSON Decode Error|js}, msg)
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

    | ConnectionError(ClosedByProcess(code, signal)) => (
        "Socket closed by GCL",
        {j|exited with code: $code
signal: $signal
|j},
      )
    | ConnectionError(DisconnectedByUser) => (
        "Disconnected",
        "Connection disconnected by ourselves",
      )
    // | ConnectionError(EmitterError) => ("Emitter Error", "")
    | ConnectionError(ShellError(error)) => (
        "Socket error",
        Util.JsError.toString(error),
      )
    | ConnectionError(ExitedByProcess(code, signal)) => (
        "GCL has crashed",
        {j|exited with code: $code
  signal: $signal
  |j},
      )
    | ConnectionError(NotEstablishedYet) => (
        "Connection not established yet",
        "Please establish the connection first",
      );
};

module Emitter = {
  type t('a) = {
    emitter: Nd.Events.t,
    emit: 'a => unit,
    once: unit => Promise.t('a),
    on: ('a => unit) => unit,
    destroy: unit => unit,
  };

  let make = () => {
    let emitter = Nd.Events.make();
    {
      emitter,
      emit: x => emitter |> Nd.Events.emit("data", x) |> ignore,
      once: () => {
        let (promise, resolve) = Promise.pending();
        emitter |> Nd.Events.once("data", resolve) |> ignore;
        promise;
      },
      on: callback => {
        emitter |> Nd.Events.on("data", callback) |> ignore;
      },
      destroy: () => Nd.Events.removeAllListeners(emitter) |> ignore,
    };
  };
};

type t = {
  mutable path: option(string),
  mutable process: option(N.ChildProcess.t),
  emitter: Emitter.t(result(Js.Json.t, Error.t)),
};

let disconnect = connection => {
  connection.emitter.destroy();
  connection.process |> Option.forEach(N.ChildProcess.kill("SIGTERM"));
  connection.process = None;

  Promise.resolved();
};

let isConnected = connection =>
  switch (connection.process) {
  | None => false
  | Some(_) => true
  };

let make = (): t => {
  {path: None, process: None, emitter: Emitter.make()};
};

let autoSearch = (name): Promise.t(result(string, Error.t)) =>
  {
    let (promise, resolve) = Promise.pending();

    // reject if the process hasn't responded for more than 1 second
    let hangTimeout =
      Js.Global.setTimeout(
        () => resolve(Error(ProcessHanging(name): Error.autoSearch)),
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
    | Error(os) => resolve(Error(NotSupported(os)))
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
            resolve(
              Error(
                NotFound(name, err |> Js.Exn.message |> Option.getOr("")),
              ),
            )
          };

          /* stderr */
          let stderr' = stderr |> Node.Buffer.toString;
          if (stderr' |> String.isEmpty |> (!)) {
            resolve(Error(NotFound(name, stderr')));
          };

          /* stdout */
          let stdout' = stdout |> Node.Buffer.toString;
          if (stdout' |> String.isEmpty) {
            resolve(Error(NotFound(name, "")));
          } else {
            resolve(Ok(stdout'));
          };
        },
      )
      |> ignore
    };

    promise;
  }
  ->Promise.mapError(e => Error.AutoSearchError(e));

let connect = connection => {
  let%Ok path = autoSearch("gcl");
  open N;
  let process = ChildProcess.spawn(path, [||], {"shell": true});
  connection.path = Some(path);
  connection.process = Some(process);

  // for incremental parsing
  let unfinishedMsg = ref(None);

  // on data
  process
  |> N.ChildProcess.stdout
  |> Nd.Stream.Readable.on(
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
           | Some(result) =>
             unfinishedMsg := None;
             connection.emitter.emit(Ok(result)) |> ignore;
           };
         },
       ),
     )
  |> ignore;

  process
  |> N.ChildProcess.stdin
  |> Nd.Stream.Writable.on(`close(() => disconnect(connection) |> ignore))
  |> ignore;

  // on errors and anomalies
  process
  |> ChildProcess.on(
       `close(
         (code, signal) => {
           connection.emitter.emit(
             Error(
               Error.ConnectionError(Error.ClosedByProcess(code, signal)),
             ),
           )
           |> ignore;
           disconnect(connection) |> ignore;
         },
       ),
     )
  |> ChildProcess.on(
       `disconnect(
         () => {
           connection.emitter.emit(
             Error(Error.ConnectionError(Error.DisconnectedByUser)),
           )
           |> ignore;
           disconnect(connection) |> ignore;
         },
       ),
     )
  |> ChildProcess.on(
       `error(
         exn => {
           connection.emitter.emit(
             Error(Error.ConnectionError(Error.ShellError(exn))),
           )
           |> ignore;
           disconnect(connection) |> ignore;
         },
       ),
     )
  |> ChildProcess.on(
       `exit(
         (code, signal) => {
           connection.emitter.emit(
             Error(
               Error.ConnectionError(Error.ExitedByProcess(code, signal)),
             ),
           )
           |> ignore;
           disconnect(connection) |> ignore;
         },
       ),
     )
  |> ignore;

  Promise.resolved(Ok());
};

let send = (request, connection): Promise.t(result(Js.Json.t, Error.t)) => {
  switch (connection.process) {
  | None =>
    Promise.resolved(Error(Error.ConnectionError(Error.NotEstablishedYet)))
  | Some(process) =>
    let promise = connection.emitter.once();

    let payload = Node.Buffer.fromString(request ++ "\n");
    // write
    process
    |> N.ChildProcess.stdin
    |> Nd.Stream.Writable.write(payload)
    |> ignore;

    promise;
  };
};
