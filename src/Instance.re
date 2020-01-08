open! Rebase;

open! Types.Instance;
module Event = Event;

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
  instance |> Response.Decoration.destroyAll;
  instance |> View.destroy;
};

// run the Tasks
let rec runTasks = (instance: t, tasks: list(Types.Task.t)): Promise.t(unit) => {
  open Types.Task;
  open Command;

  let runTask =
    fun
    | WithInstance(callback) =>
      callback(instance)->Promise.flatMap(runTasks(instance))
    | DispatchRemote(command) => {
        instance.history = Some(command);
        Remote.dispatch(command) |> runTasks(instance);
      }
    | DispatchLocal(command) =>
      Local.dispatch(command) |> runTasks(instance)
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
