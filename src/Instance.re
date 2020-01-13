open! Rebase;

open! Types.Instance;

let make = (editor: Atom.TextEditor.t): t => {
  editor,
  view: View.make(editor),
  toggle: false,
  connection: Connection.make(),
  decorations: [||],
  specifications: [||],
  history: None,
};

module View = {
  // destroy the view
  let destroy = instance => instance.editor |> View.destroy;
  let show = instance => instance.view.setActivation(true) |> ignore;
  let hide = instance => instance.view.setActivation(false) |> ignore;

  let displayError = (header, body, instance) => {
    instance.view.setHeader(Error(header)) |> ignore;
    instance.view.setBody(Plain(body)) |> ignore;
  };
};

module Connection_ = {
  // destroy the connection
  let disconnect = instance =>
    Connection.disconnect(instance.connection) |> ignore;

  // connect if not connected yet
  let getConnection =
      (instance): Promise.t(result(Connection.t, Connection.Error.t)) => {
    let isConnected = Connection.isConnected(instance.connection);
    if (isConnected) {
      Promise.resolved(Ok(instance.connection));
    } else {
      Connection.connect(instance.connection)
      ->Promise.mapOk(() => instance.connection);
    };
  };

  let sendRequest =
      (request, instance): Promise.t(result(Response.t, Connection.Error.t)) => {
    let value = Request.encode(request);
    Js.log("=== Send ===");
    Js.log2("[ json ]", value);

    let%Ok conn = instance->getConnection;
    let%Ok result = Connection.send(value, conn);

    Js.log("=== Recieved ===");
    Js.log2("[ json ]", result);
    Js.log2("[ value ]", result |> Response.decode);
    Promise.resolved(Ok(Response.decode(result)));
  };
};

let destroy = instance => {
  instance |> Connection_.disconnect;
  instance.decorations |> Array.forEach(Atom.Decoration.destroy);
  instance |> View.destroy;
};
//
// instance.connection.emitter.on(
//   fun
//   | Ok(_) => []
//   | Error(error) => {
//       let (header, body) = Connection.Error.toString(error);
//       [Display(Error(header), Plain(body))];
//     },
// );

module Command_ = {
  open Types.Command;
  open Types.Task;
  // from Editor Command to Tasks
  module Local = {
    let dispatch =
      fun
      | Toggle => [
          WithInstance(
            instance =>
              if (instance.Types.Instance.toggle) {
                instance.toggle = false;
                instance.view.setActivation(false) |> ignore;
                // destroy all decorations
                instance.decorations |> Array.forEach(Atom.Decoration.destroy);
                // destroy the connection
                instance.connection |> Connection.disconnect |> ignore;

                Promise.resolved([]);
              } else {
                instance.toggle = true;
                instance.view.setActivation(true) |> ignore;

                if (Connection.isConnected(instance.connection)) {
                  Promise.resolved([]);
                } else {
                  instance
                  ->Connection_.getConnection
                  ->Promise.map(
                      fun
                      | Ok(_) => [DispatchLocal(Save)]
                      | Error(error) => {
                          let (header, body) =
                            Connection.Error.toString(error);
                          [Display(Error(header), Plain(body))];
                        },
                    );
                };
              },
          ),
        ]
      | Save => [
          WithInstance(
            instance => {
              instance.decorations |> Array.forEach(Atom.Decoration.destroy);
              instance.editor
              ->Atom.TextEditor.save
              ->Promise.Js.fromBsPromise
              ->Promise.Js.catch(_ => Promise.resolved())
              ->Promise.map(() => {
                  let filepath = Atom.TextEditor.getPath(instance.editor);
                  switch (filepath) {
                  | Some(path) => [DispatchRemote(Load(path))]
                  | None => [
                      Display(
                        Error("Cannot read filepath"),
                        Plain("Please save the file first"),
                      ),
                    ]
                  };
                });
            },
          ),
        ]
      | Refine => [
          WithInstance(
            instance =>
              Spec.fromCursorPosition(instance)
              |> Option.mapOr(
                   spec => Promise.resolved([DispatchRemote(Refine(spec))]),
                   Promise.resolved([]),
                 ),
          ),
        ]
      | Debug => [DispatchRemote(Debug)];
  };

  module Remote = {
    // from Editor Command to Tasks
    let dispatch =
      fun
      | Load(path) => [SendRequest(Load(path))]
      | Refine(spec) => [
          WithInstance(
            instance => {
              open Specification;
              let payload = Spec.getPayload(spec, instance);
              Js.log2("[refine]", spec.range);
              Promise.resolved([SendRequest(Refine(spec.id, payload))]);
            },
          ),
        ]
      | Debug => [SendRequest(Debug)];
  };
};

// run the Tasks
let rec runTasks = (instance: t, tasks: list(Types.Task.t)): Promise.t(unit) => {
  open Types.Task;

  let runTask =
    fun
    | WithInstance(callback) =>
      callback(instance)->Promise.flatMap(runTasks(instance))
    | AddDecorations(callback) => {
        instance.decorations =
          Array.concat(
            callback(instance.specifications, instance.editor),
            instance.decorations,
          );
        Promise.resolved();
      }
    | SetSpecifications(specs) => {
        instance.specifications = specs;
        Promise.resolved();
      }
    | DispatchRemote(command) => {
        instance.history = Some(command);
        Command_.Remote.dispatch(command) |> runTasks(instance);
      }
    | DispatchLocal(command) =>
      Command_.Local.dispatch(command) |> runTasks(instance)
    | SendRequest(request) =>
      Connection_.sendRequest(request, instance)
      ->Promise.flatMap(
          fun
          | Error(error) => {
              let (header, body) = Connection.Error.toString(error);
              instance |> View.displayError(header, body);
              Promise.resolved();
            }
          | Ok(x) => x |> Response.handle |> runTasks(instance),
        )
    | Display(header, body) => {
        instance.view.setHeader(header) |> ignore;
        instance.view.setBody(body) |> ignore;
        Promise.resolved();
      };

  (tasks |> List.map(runTask) |> Util.Promise.each)->Promise.map(_ => ());
};
