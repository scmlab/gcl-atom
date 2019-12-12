open Rebase;
open Async;

open! Type.Instance;
module Event = Event;

let make = (editor: Atom.TextEditor.t): t => {
  editor,
  view: View.make(editor),
  toggle: false,
  connection: Connection.make(),
  decorations: [||],
  specifications: [||],
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
    |> thenOk(Connection.send(Request.encode(request)))
    |> mapError(error => {
         let (header, body) = Connection.Error.toString(error);
         instance |> View.displayError(header, body);
       })
    |> mapOk(result => {
         Js.log2("[ received json ]", result);
         Js.log2("[ received value ]", result |> Response.decode);
         Response.decode(result);
       });
  };
};

let destroy = instance => {
  instance |> Connection.disconnect;
  instance |> Decoration.destroyAll;
  instance |> View.destroy;
};

let handle = (error): list(Command.task) => {
  let handleError = _site => {
    open Response.Error;
    open Command;
    ();
    fun
    | LexicalError(point) => [
        WithInstance(
          instance => {
            instance |> Decoration.markError(point);
            resolve([]);
          },
        ),
        Display(
          Error("Lexical Error"),
          Plain(
            "at "
            ++ string_of_int(Atom.Point.row(point))
            ++ ","
            ++ string_of_int(Atom.Point.column(point)),
          ),
        ),
      ]
    | SyntacticError({location, message}) => [
        WithInstance(
          instance => {
            instance |> Decoration.markError'(location);
            resolve([]);
          },
        ),
        Display(Error("Parse Error"), Plain(message)),
      ]
    | ConvertError(MissingBound(range)) => [
        WithInstance(
          instance => {
            instance |> Decoration.highlightError(range);
            resolve([]);
          },
        ),
        Display(
          Error("Bound Missing"),
          Plain(
            "Bound missing at the end of the assertion before the DO construct \" , bnd : ... }\"",
          ),
        ),
      ]
    | ConvertError(MissingAssertion(range)) => [
        WithInstance(
          instance => {
            instance |> Decoration.highlightError(range);
            resolve([]);
          },
        ),
        Display(
          Error("Assertion Missing"),
          Plain("Assertion before the DO construct is missing"),
        ),
      ]
    | ConvertError(ExcessBound(range)) => [
        WithInstance(
          instance => {
            instance |> Decoration.highlightError(range);
            resolve([]);
          },
        ),
        Display(
          Error("Excess Bound"),
          Plain("Unnecessary bound annotation at this assertion"),
        ),
      ]
    | ConvertError(MissingPostcondition) => [
        Display(
          Error("Postcondition Missing"),
          Plain("The last statement of the program should be an assertion"),
        ),
      ]
    | ConvertError(DigHole(range)) => [
        WithInstance(
          instance =>
            instance
            |> Decoration.digHole(range)
            |> thenOk(() => resolve([DispatchLocal(Command.Save)])),
        ),
      ]
    | ConvertError(Panic(message)) => [
        Display(
          Error("Panic"),
          Plain(
            "This should not have happened, please report this issue\n"
            ++ message,
          ),
        ),
      ];
  };

  let handleError' = ((site, error)) => handleError(site, error);

  switch (error) {
  | Response.Error(pairs) =>
    pairs |> Array.map(handleError') |> List.fromArray |> Js.List.flatten
  | OK(obligations, specifications) => [
      WithInstance(
        instance => {
          specifications
          |> Array.forEach(Fn.flip(Decoration.markSpec, instance));
          instance.specifications = specifications;
          resolve([]);
        },
      ),
      Display(Plain("Proof Obligations"), ProofObligations(obligations)),
    ]
  | Resolve(i) => [
      WithInstance(
        instance => {
          Spec.resolve(i, instance);
          resolve([]);
        },
      ),
    ]
  | UnknownResponse(json) => [
      Display(
        Error("Panic: unknown response from GCL"),
        Plain(Js.Json.stringify(json)),
      ),
    ]
  };
};

let rec runTasks =
        (instance: t, tasks: list(Command.task)): Async.t(unit, unit) => {
  open Command;
  let runTask =
    fun
    | WithInstance(callback) =>
      callback(instance) |> thenOk(runTasks(instance))
    | DispatchRemote(command) =>
      Remote.dispatch(command) |> runTasks(instance)
    | DispatchLocal(command) =>
      Local.dispatch(command) |> runTasks(instance)
    | SendRequest(request) =>
      instance
      |> Connection.sendRequest(request)
      |> thenOk(x => x |> handle |> runTasks(instance))
    | Display(header, body) => {
        instance.view.setHeader(header) |> ignore;
        instance.view.setBody(body) |> ignore;
        resolve();
      };

  tasks
  |> List.map(runTask)
  |> Array.fromList
  |> Js.Promise.all
  |> fromPromise
  |> mapError(_ => ())
  |> thenOk(_ => resolve());
} /*   */;
