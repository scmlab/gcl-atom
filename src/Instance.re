open Rebase;
open Async;

open! Type.Instance;
module Event = Event;

let make = (editor: Atom.TextEditor.t): t => {
  // add "gcl" to the class-list
  editor
  |> Atom.Views.getView
  |> Webapi.Dom.HtmlElement.classList
  |> Webapi.Dom.DomTokenList.add("gcl");

  let view = View.make(editor);
  let connection = Connection.make();

  {
    editor,
    view,
    toggle: false,
    connection,
    decorations: [||],
    specifications: [||],
  };
};

let destroy = instance => {
  // remove "gcl" from the class-list of the editor
  instance.editor
  |> Atom.Views.getView
  |> Webapi.Dom.HtmlElement.classList
  |> Webapi.Dom.DomTokenList.remove("gcl");
  // destroy the connection
  Connection.disconnect(instance.connection) |> ignore;
  // destroy all decorations
  instance.decorations |> Array.forEach(Atom.Decoration.destroy);
  // destroy the view
  instance.editor |> View.destroy;
};

let showView = instance => instance.view.setActivation(true) |> ignore;

let hideView = instance => instance.view.setActivation(false) |> ignore;

// connect if not connected yet
let getConnection = instance =>
  if (Connection.isConnected(instance.connection)) {
    resolve(instance.connection);
  } else {
    Connection.connect(instance.connection)
    |> thenError(error => {
         let (header, body) = Connection.Error.toString(error);
         instance.view.setHeader(Error(header)) |> ignore;
         instance.view.setBody(Plain(body)) |> ignore;
         resolve();
       })
    |> thenOk(_ => resolve(instance.connection));
  };

let sendRequest = (request, instance) => {
  instance
  |> getConnection
  |> thenOk(Connection.send(Request.encode(request)))
  |> mapError(error => {
       let (header, body) = Connection.Error.toString(error);
       instance.view.setHeader(Error(header)) |> ignore;
       instance.view.setBody(Plain(body)) |> ignore;
       ();
     })
  |> mapOk(result => {
       Js.log2("[ received json ]", result);
       Js.log2("[ received value ]", result |> Response.decode);
       Response.decode(result);
     });
};

let handle =
  Command.(
    fun
    | Response.Error(Response.Error.LexicalError(point)) => {
        [
          WithInstance(
            instance => {
              instance |> Handler.markError(point);
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
        ];
      }
    | Error(SyntacticError(errors)) => {
        // TODO: reporting only the first error now
        switch (errors[0]) {
        | None => [Display(AllGood, Nothing)]
        | Some({locations, message}) => [
            WithInstance(
              instance => {
                locations
                |> Array.forEach(range =>
                     instance |> Handler.markError'(range)
                   );
                resolve([]);
              },
            ),
            Display(Error("Parse Error"), Plain(message)),
          ]
        };
      }
    | Error(TransformError(MissingBound(range))) => {
        [
          WithInstance(
            instance => {
              instance |> Handler.highlightError(range);
              resolve([]);
            },
          ),
          Display(
            Error("Bound Missing"),
            Plain(
              "Bound missing at the end of the assertion before the DO construct \" , bnd : ... }\"",
            ),
          ),
        ];
      }
    | Error(TransformError(MissingAssertion(range))) => {
        [
          WithInstance(
            instance => {
              instance |> Handler.highlightError(range);
              resolve([]);
            },
          ),
          Display(
            Error("Assertion Missing"),
            Plain("Assertion before the DO construct is missing"),
          ),
        ];
      }
    | Error(TransformError(ExcessBound(range))) => {
        [
          WithInstance(
            instance => {
              instance |> Handler.highlightError(range);
              resolve([]);
            },
          ),
          Display(
            Error("Excess Bound"),
            Plain("Unnecessary bound annotation at this assertion"),
          ),
        ];
      }
    | Error(TransformError(MissingPostcondition)) => {
        [
          Display(
            Error("Postcondition Missing"),
            Plain("The last statement of the program should be an assertion"),
          ),
        ];
      }
    | Error(TransformError(DigHole(range))) => {
        [
          WithInstance(
            instance =>
              instance
              |> Handler.digHole(range)
              |> thenOk(() => resolve([DispatchRaw(Command.Save)])),
          ),
        ];
      }
    | Error(TransformError(Panic(message))) => {
        [
          Display(
            Error("Panic"),
            Plain(
              "This should not have happened, please report this issue\n"
              ++ message,
            ),
          ),
        ];
      }
    | OK(obligations, specifications) => {
        [
          WithInstance(
            instance => {
              specifications
              |> Array.forEach(Fn.flip(Handler.markSpec, instance));
              instance.specifications = specifications;
              resolve([]);
            },
          ),
          Display(
            Plain("Proof Obligations"),
            ProofObligations(obligations),
          ),
        ];
      }
    | Resolve(i) => {
        [
          WithInstance(
            instance => {
              Handler.Spec.resolve(i, instance);
              resolve([]);
            },
          ),
        ];
      }
    | UnknownResponse(json) => {
        [
          Display(
            Error("Panic: unknown response from GCL"),
            Plain(Js.Json.stringify(json)),
          ),
        ];
      }
  );

let rec runTasks =
        (instance: t, tasks: list(Command.task)): Async.t(unit, unit) => {
  open Command;
  let runTask =
    fun
    | WithInstance(callback) =>
      callback(instance) |> thenOk(runTasks(instance))
    | Dispatch(command) =>
      Elaborated.dispatch(command) |> runTasks(instance)
    | DispatchRaw(command) => Raw.dispatch(command) |> runTasks(instance)
    | SendRequest(request) =>
      instance
      |> sendRequest(request)
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
};
