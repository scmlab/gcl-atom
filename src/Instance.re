open Rebase;
open Async;

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

module Connection = {
  // destroy the connection
  let disconnect = instance =>
    Connection.disconnect(instance.connection) |> ignore;
  // connect if not connected yet
  let getConnection = (instance): Async.t(Connection.t, Connection.Error.t) => {
    let isConnected = Connection.isConnected(instance.connection);
    if (isConnected) {
      resolve(instance.connection);
    } else {
      Connection.connect(instance.connection)
      |> thenOk(() => resolve(instance.connection));
    };
  };

  let sendRequest = (request, instance): Async.t(Response.t, unit) => {
    instance
    |> getConnection
    |> thenOk(conn => {
         let value = Request.encode(request);
         Js.log("=== Send ===");
         Js.log2("[ json ]", value);
         conn |> Connection.send(value);
       })
    |> mapError(error => {
         let (header, body) = Connection.Error.toString(error);
         instance |> View.displayError(header, body);
       })
    |> mapOk(result => {
         Js.log("=== Recieved ===");
         Js.log2("[ json ]", result);
         Js.log2("[ value ]", result |> Response.decode);
         Response.decode(result);
       });
  };
};

let destroy = instance => {
  instance |> Connection.disconnect;
  instance |> Response.Decoration.destroyAll;
  instance |> View.destroy;
};

// run the Tasks
let rec runTasks =
        (instance: t, tasks: list(Types.Task.t)): Async.t(unit, unit) => {
  open Types.Task;
  open Command;

  let runTask =
    fun
    | WithInstance(callback) =>
      callback(instance) |> thenOk(runTasks(instance))
    | DispatchRemote(command) => {
        instance.history = Some(command);
        Remote.dispatch(command) |> runTasks(instance);
      }
    | DispatchLocal(command) =>
      Local.dispatch(command) |> runTasks(instance)
    | SendRequest(request) =>
      instance
      |> Connection.sendRequest(request)
      |> thenOk(x => x |> Response.handle |> runTasks(instance))
    | Display(header, body) => {
        instance.view.setHeader(header) |> ignore;
        instance.view.setBody(body) |> ignore;
        resolve();
      };

  tasks |> List.map((x, ()) => runTask(x)) |> each |> thenOk(_ => resolve());
};
