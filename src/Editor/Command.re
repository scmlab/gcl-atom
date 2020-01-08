open! Rebase;

module Local = {
  open Types.Command;
  open Types.Task;
  type t = Types.Command.local;
  let commandNames = [|"toggle", "save", "refine"|];
  let parse =
    fun
    | "toggle" => Toggle
    | "save" => Save
    | "refine" => Refine
    | _ => Save;

  // from Editor Command to Tasks
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
                instance.connection
                ->Connection.connect
                ->Promise.map(
                    fun
                    | Ok () => [DispatchLocal(Save)]
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
            Response.Spec.fromCursorPosition(instance)
            |> Option.mapOr(
                 spec => Promise.resolved([DispatchRemote(Refine(spec))]),
                 Promise.resolved([]),
               ),
        ),
      ];
};

module Remote = {
  open Types.Command;
  open Types.Task;
  type t = Types.Command.remote;
  // from Editor Command to Tasks
  let dispatch =
    fun
    | Load(path) => [SendRequest(Load(path))]
    | Refine(spec) => [
        WithInstance(
          instance => {
            open Specification;
            let payload = Response.Spec.getPayload(spec, instance);
            Js.log2("[refine]", spec.range);
            Promise.resolved([SendRequest(Refine(spec.id, payload))]);
          },
        ),
      ];
};
