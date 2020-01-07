open Async;
open Rebase;

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

              resolve([]);
            } else {
              instance.toggle = true;
              instance.view.setActivation(true) |> ignore;

              if (Connection.isConnected(instance.connection)) {
                resolve([]);
              } else {
                Connection.connect(instance.connection)
                |> Async.then_(
                     () => resolve([DispatchLocal(Save)]),
                     error => {
                       let (header, body) = Connection.Error.toString(error);
                       resolve([Display(Error(header), Plain(body))]);
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
            |> Atom.TextEditor.save
            |> fromPromise
            |> mapError(_ => ())
            |> thenOk(_ => {
                 let filepath = Atom.TextEditor.getPath(instance.editor);
                 switch (filepath) {
                 | Some(path) => resolve([DispatchRemote(Load(path))])
                 | None =>
                   resolve([
                     Display(
                       Error("Cannot read filepath"),
                       Plain("Please save the file first"),
                     ),
                   ])
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
                 spec => resolve([DispatchRemote(Refine(spec))]),
                 resolve([]),
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
            resolve([SendRequest(Refine(spec.id, payload))]);
          },
        ),
      ];
};
