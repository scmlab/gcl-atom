open! Rebase;
open Types.Command;
open Task__Types;

// from Editor Command to Tasks
let dispatch =
  fun
  | Toggle => [
      WithInstance(
        instance =>
          if (instance.Instance.toggle) {
            instance.toggle = false;
            instance.view.setActivation(false) |> ignore;
            // destroy all decorations
            instance.decorations |> Array.forEach(Atom.Decoration.destroy);
            // destroy the connection
            instance |> Instance.Connection_.destroy;

            Promise.resolved([]);
          } else {
            instance.toggle = true;
            instance.view.setActivation(true) |> ignore;

            switch (instance.connection) {
            | Some(_) => Promise.resolved([])
            | None =>
              instance
              ->Instance.Connection_.establish
              ->Promise.map(
                  fun
                  | Ok(_) => [DispatchCommand(Save)]
                  | Error(error) => {
                      let (header, body) = Instance.Error.toString(error);
                      [Display(Error(header), Plain(body))];
                    },
                )
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
              | Some(path) => [SendRequest(Load(path))]
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
      DispatchCommand(Save),
      WithInstance(
        instance =>
          Spec.fromCursorPosition(instance)
          |> Option.mapOr(
               spec => {
                 let payload = Spec.getPayload(spec, instance);
                 Promise.resolved([SendRequest(Refine(spec.id, payload))]);
               },
               Promise.resolved([]),
             ),
      ),
    ]
  | InsertAssertion => [
      DispatchCommand(Save),
      WithInstance(
        instance => {
          let cursor =
            Atom.TextEditor.getCursorBufferPosition(instance.editor);
          open Base.Pos;
          let Pos(_, line, _) = Base.Pos.fromPoint("whatever", cursor);
          Promise.resolved([SendRequest(InsertAssertion(line))]);
        },
      ),
    ]
  | Debug => [SendRequest(Debug)];
