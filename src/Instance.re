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

  {editor, view, connection, decorations: [||]};
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

let activate = instance => instance.Type.view.setActivation(true);

let deactivate = instance => instance.Type.view.setActivation(false);

let rec dispatch = (request, instance) => {
  Command.(
    switch (request) {
    | Activate =>
      activate(instance) |> ignore;
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
    | Deactivate => deactivate(instance)
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
             Connection.send(
               Request.encode(Request.Load(path)),
               instance.connection,
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
    }
  );
}
and handle = (instance: Type.instance) =>
  fun
  | ParseError(errors) => {
      // TODO: reporting only the first error now
      switch (errors[0]) {
      | None =>
        instance.view.setHeader(AllGood) |> ignore;
        instance.view.setBody(Nothing) |> ignore;
        Async.resolve();
      | Some((pos, msg)) =>
        instance.view.setHeader(Error("Parse Error")) |> ignore;
        instance.view.setBody(Plain(msg)) |> ignore;

        let range = Atom.Range.make(pos, pos);
        instance |> Handler.markLineError(range);

        Async.resolve();
      };
    }
  | SyntaxError(MissingBound(range)) => {
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
  | SyntaxError(MissingAssertion(range)) => {
      instance.view.setHeader(Error("Assertion Missing")) |> ignore;
      instance.view.setBody(
        Plain("Assertion before the DO construct is missing"),
      )
      |> ignore;
      instance |> Handler.highlightError(range);
      Async.resolve();
    }
  | SyntaxError(ExcessBound(range)) => {
      instance.view.setHeader(Error("Excess Bound")) |> ignore;
      instance.view.setBody(
        Plain("Unnecessary bound annotation at this assertion"),
      )
      |> ignore;
      instance |> Handler.highlightError(range);
      Async.resolve();
    }
  | SyntaxError(MissingPostcondition) => {
      instance.view.setHeader(Error("Postcondition Missing")) |> ignore;
      instance.view.setBody(
        Plain("The last statement of the program should be an assertion"),
      )
      |> ignore;
      Async.resolve();
    }
  | SyntaxError(DigHole(range)) => {
      instance
      |> Handler.digHole(range)
      |> Async.thenOk(() => dispatch(Save, instance));
    }
  | SyntaxError(Panic(message)) => {
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
      Async.resolve();
    }
  | UnknownResponse(json) => {
      instance.view.setHeader(Error("Panic: unknown response from GCL"))
      |> ignore;
      instance.view.setBody(Plain(Js.Json.stringify(json))) |> ignore;
      Async.resolve();
    };
