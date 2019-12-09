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

let rec dispatchRaw = (request, instance): Async.t(Command.output, unit) => {
  Command.(
    switch (request) {
    | Raw.Toggle =>
      if (instance.toggle) {
        resolve(Dispatch(Activate));
      } else {
        resolve(Dispatch(Deactivate));
      }
    | Save =>
      instance.decorations |> Array.forEach(Atom.Decoration.destroy);
      instance.editor
      |> Atom.TextEditor.save
      |> fromPromise
      |> mapError(_ => ())
      |> thenOk(_ => {
           let filepath = Atom.TextEditor.getPath(instance.editor);
           switch (filepath) {
           | Some(path) => resolve(Dispatch(Update(path)))
           | None =>
             resolve(
               Display(
                 Error("Cannot read filepath"),
                 Plain("Please save the file first"),
               ),
             )
           };
         });
    | Refine =>
      Handler.Spec.fromCursorPosition(instance)
      |> Option.mapOr(
           spec => resolve(Dispatch(Refine(spec))),
           resolve(Noop),
         )
    }
  );
}
and dispatch = (request, instance): Async.t(Command.output, unit) => {
  Command.(
    switch (request) {
    | Activate =>
      instance.toggle = false;
      hideView(instance);
      // destroy all decorations
      instance.decorations |> Array.forEach(Atom.Decoration.destroy);
      // destroy the connection
      instance.connection |> Connection.disconnect |> ignore;

      resolve(Noop);
    | Deactivate =>
      instance.toggle = true;
      showView(instance);
      // reconnect if already connected
      if (Connection.isConnected(instance.connection)) {
        resolve(Noop);
      } else {
        Connection.connect(instance.connection)
        |> thenError(error => {
             let (header, body) = Connection.Error.toString(error);
             instance.view.setHeader(Error(header)) |> ignore;
             instance.view.setBody(Plain(body)) |> ignore;
             resolve();
           })
        |> thenOk(_ => dispatchRaw(Raw.Save, instance));
      };
    | Update(path) =>
      instance
      |> sendRequest(Request.Load(path))
      |> thenOk(handle(instance))
    | Refine(spec) =>
      open Response.Specification;
      let payload = Handler.Spec.getPayload(spec, instance);
      instance
      |> sendRequest(Refine(spec.id, payload))
      |> thenOk(handle(instance));
    }
  );
}
and handle = instance =>
  Command.(
    fun
    | Error(LexicalError(point)) => {
        instance.view.setHeader(Error("Lexical Error")) |> ignore;
        instance.view.setBody(
          Plain(
            "at "
            ++ string_of_int(Atom.Point.row(point))
            ++ ","
            ++ string_of_int(Atom.Point.column(point)),
          ),
        )
        |> ignore;

        instance |> Handler.markError(point);

        resolve(Noop);
      }
    | Error(SyntacticError(errors)) => {
        // TODO: reporting only the first error now
        switch (errors[0]) {
        | None => resolve(Display(AllGood, Nothing))
        | Some({locations, message}) =>
          locations
          |> Array.forEach(range => instance |> Handler.markError'(range));
          resolve(Display(Error("Parse Error"), Plain(message)));
        };
      }
    | Error(TransformError(MissingBound(range))) => {
        instance |> Handler.highlightError(range);
        resolve(
          Display(
            Error("Bound Missing"),
            Plain(
              "Bound missing at the end of the assertion before the DO construct \" , bnd : ... }\"",
            ),
          ),
        );
      }
    | Error(TransformError(MissingAssertion(range))) => {
        instance |> Handler.highlightError(range);
        resolve(
          Display(
            Error("Assertion Missing"),
            Plain("Assertion before the DO construct is missing"),
          ),
        );
      }
    | Error(TransformError(ExcessBound(range))) => {
        instance |> Handler.highlightError(range);
        resolve(
          Display(
            Error("Excess Bound"),
            Plain("Unnecessary bound annotation at this assertion"),
          ),
        );
      }
    | Error(TransformError(MissingPostcondition)) => {
        resolve(
          Display(
            Error("Postcondition Missing"),
            Plain("The last statement of the program should be an assertion"),
          ),
        );
      }
    | Error(TransformError(DigHole(range))) => {
        instance
        |> Handler.digHole(range)
        |> thenOk(() => dispatchRaw(Command.Raw.Save, instance));
      }
    | Error(TransformError(Panic(message))) => {
        resolve(
          Display(
            Error("Panic"),
            Plain(
              "This should not have happened, please report this issue\n"
              ++ message,
            ),
          ),
        );
      }
    | OK(obligations, specifications) => {
        specifications |> Array.forEach(Fn.flip(Handler.markSpec, instance));
        instance.specifications = specifications;
        resolve(
          Display(
            Plain("Proof Obligations"),
            ProofObligations(obligations),
          ),
        );
      }
    | Resolve(i) => {
        Handler.Spec.resolve(i, instance);
        resolve(Noop);
      }
    | UnknownResponse(json) => {
        resolve(
          Display(
            Error("Panic: unknown response from GCL"),
            Plain(Js.Json.stringify(json)),
          ),
        );
      }
  );

let rec run = (instance: t, output: Command.output): Async.t(unit, unit) => {
  switch (output) {
  | Noop => resolve()
  | Dispatch(command) =>
    dispatch(command, instance) |> thenOk(run(instance))
  | Display(header, body) =>
    instance.view.setHeader(header) |> ignore;
    instance.view.setBody(body) |> ignore;
    resolve();
  };
};
