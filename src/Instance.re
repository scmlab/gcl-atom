open Rebase;
open Async;

module Event = Event;

let make = (editor: Atom.TextEditor.t): Type.instance => {
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
  instance.Type.editor
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

let showView = instance => instance.Type.view.setActivation(true) |> ignore;

let hideView = instance => instance.Type.view.setActivation(false) |> ignore;

// connect if not connected yet
let getConnection = instance =>
  if (Connection.isConnected(instance.Type.connection)) {
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

let rec dispatch = (request, instance) => {
  Command.(
    switch (request) {
    | Toggle =>
      if (instance.Type.toggle) {
        instance.toggle = false;
        hideView(instance);
        // destroy all decorations
        instance.decorations |> Array.forEach(Atom.Decoration.destroy);
        // destroy the connection
        instance.connection |> Connection.disconnect |> ignore;

        resolve();
      } else {
        instance.toggle = true;
        showView(instance);
        // reconnect if already connected
        if (Connection.isConnected(instance.connection)) {
          resolve();
        } else {
          Connection.connect(instance.connection)
          |> thenError(error => {
               let (header, body) = Connection.Error.toString(error);
               instance.view.setHeader(Error(header)) |> ignore;
               instance.view.setBody(Plain(body)) |> ignore;
               resolve();
             })
          |> thenOk(_ => dispatch(Save, instance));
        };
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
           | Some(path) =>
             instance
             |> getConnection
             |> thenOk(
                  Connection.send(Request.encode(Request.Load(path))),
                )
             |> Async.mapError(error => {
                  let (header, body) = Connection.Error.toString(error);
                  instance.view.setHeader(Error(header)) |> ignore;
                  instance.view.setBody(Plain(body)) |> ignore;
                  ();
                })
           | None =>
             instance.view.setHeader(Error("Cannot read filepath "))
             |> ignore;
             instance.view.setBody(Plain("Please save the file first"))
             |> ignore;
             reject();
           };
         })
      |> thenOk(result => {
           Js.log2("[ received json ]", result);
           Js.log2("[ received value ]", result |> Response.decode);
           Response.decode(result) |> handle(instance);
         });
    | Refine =>
      Handler.refine(instance) |> thenOk(() => dispatch(Save, instance))
    }
  );
}
and handle = (instance: Type.instance) =>
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

      Async.resolve();
    }
  | Error(SyntacticError(errors)) => {
      // TODO: reporting only the first error now
      switch (errors[0]) {
      | None =>
        instance.view.setHeader(AllGood) |> ignore;
        instance.view.setBody(Nothing) |> ignore;
        Async.resolve();
      | Some({locations, message}) =>
        instance.view.setHeader(Error("Parse Error")) |> ignore;
        instance.view.setBody(Plain(message)) |> ignore;
        locations
        |> Array.forEach(range => instance |> Handler.markError'(range));

        Async.resolve();
      };
    }
  | Error(TransformError(MissingBound(range))) => {
      instance.view.setHeader(Error("Bound Missing")) |> ignore;
      instance.view.setBody(
        Plain(
          "Bound missing at the end of the assertion before the DO construct \" , bnd : ... }\"",
        ),
      )
      |> ignore;
      instance |> Handler.highlightError(range);
      Async.resolve();
    }
  | Error(TransformError(MissingAssertion(range))) => {
      instance.view.setHeader(Error("Assertion Missing")) |> ignore;
      instance.view.setBody(
        Plain("Assertion before the DO construct is missing"),
      )
      |> ignore;
      instance |> Handler.highlightError(range);
      Async.resolve();
    }
  | Error(TransformError(ExcessBound(range))) => {
      instance.view.setHeader(Error("Excess Bound")) |> ignore;
      instance.view.setBody(
        Plain("Unnecessary bound annotation at this assertion"),
      )
      |> ignore;
      instance |> Handler.highlightError(range);
      Async.resolve();
    }
  | Error(TransformError(MissingPostcondition)) => {
      instance.view.setHeader(Error("Postcondition Missing")) |> ignore;
      instance.view.setBody(
        Plain("The last statement of the program should be an assertion"),
      )
      |> ignore;
      Async.resolve();
    }
  | Error(TransformError(DigHole(range))) => {
      instance
      |> Handler.digHole(range)
      |> Async.thenOk(() => dispatch(Save, instance));
    }
  | Error(TransformError(Panic(message))) => {
      instance.view.setHeader(Error("Panic")) |> ignore;
      instance.view.setBody(
        Plain(
          "This should not have happened, please report this issue\n"
          ++ message,
        ),
      )
      |> ignore;
      Async.resolve();
    }
  | OK(obligations, specifications) => {
      instance.view.setHeader(Plain("Proof Obligations")) |> ignore;
      instance.view.setBody(ProofObligations(obligations)) |> ignore;
      specifications |> Array.forEach(Fn.flip(Handler.markSpec, instance));
      instance.specifications = specifications;
      Async.resolve();
    }
  | UnknownResponse(json) => {
      instance.view.setHeader(Error("Panic: unknown response from GCL"))
      |> ignore;
      instance.view.setBody(Plain(Js.Json.stringify(json))) |> ignore;
      Async.resolve();
    };
