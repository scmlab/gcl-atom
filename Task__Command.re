open Rebase;
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
                    | Ok(_) => [DispatchLocal(Save)]
                    | Error(error) => {
                        let (header, body) = GCL.Error.toString(error);
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
        DispatchLocal(Save),
        WithInstance(
          instance =>
            Spec.fromCursorPosition(instance)
            |> Option.mapOr(
                 spec => Promise.resolved([DispatchRemote(Refine(spec))]),
                 Promise.resolved([]),
               ),
        ),
      ]
    | InsertAssertion => [
        DispatchLocal(Save),
        WithInstance(
          instance => {
            let cursor =
              Atom.TextEditor.getCursorBufferPosition(instance.editor);
            open Base.Pos;
            let Pos(_, line, _) = Base.Pos.fromPoint("whatever", cursor);
            Promise.resolved([DispatchRemote(InsertAssertion(line))]);
          },
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
            open GCL.Response.Specification;
            let payload = Spec.getPayload(spec, instance);
            Promise.resolved([SendRequest(Refine(spec.id, payload))]);
          },
        ),
      ]
    | InsertAssertion(n) => [SendRequest(InsertAssertion(n))]
    | Debug => [SendRequest(Debug)];
};
