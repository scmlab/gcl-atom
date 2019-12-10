open Async;
open Rebase;

type elaborated =
  | Activate
  | Deactivate
  | Update(string)
  | Refine(Response.Specification.t)
and raw =
  | Toggle
  | Save
  | Refine
and task =
  | WithInstance(Type.Instance.t => Async.t(list(task), unit))
  | Dispatch(elaborated)
  | DispatchRaw(raw)
  | SendRequest(Request.t)
  | Display(Type.View.header, Body.t);

module Raw = {
  type t = raw;
  let commandNames = [|"toggle", "save", "refine"|];
  let parse =
    fun
    | "toggle" => Toggle
    | "save" => Save
    | "refine" => Refine
    | _ => Save;
  let dispatch =
    fun
    | Toggle => [
        WithInstance(
          instance =>
            if (instance.toggle) {
              resolve([Dispatch(Activate)]);
            } else {
              resolve([Dispatch(Deactivate)]);
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
                 | Some(path) => resolve([Dispatch(Update(path))])
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
            Handler.Spec.fromCursorPosition(instance)
            |> Option.mapOr(
                 spec => resolve([Dispatch(Refine(spec))]),
                 resolve([]),
               ),
        ),
      ];
};

module Elaborated = {
  type t = elaborated;
  let dispatch =
    fun
    | Activate => [
        WithInstance(
          instance => {
            instance.toggle = false;
            instance.view.setActivation(false) |> ignore;
            // destroy all decorations
            instance.decorations |> Array.forEach(Atom.Decoration.destroy);
            // destroy the connection
            instance.connection |> Connection.disconnect |> ignore;

            resolve([]);
          },
        ),
      ]
    | Deactivate => [
        WithInstance(
          instance => {
            instance.toggle = true;
            instance.view.setActivation(true) |> ignore;

            if (Connection.isConnected(instance.connection)) {
              resolve([]);
            } else {
              Connection.connect(instance.connection)
              |> Async.then_(
                   () => resolve([DispatchRaw(Save)]),
                   error => {
                     let (header, body) = Connection.Error.toString(error);
                     resolve([Display(Error(header), Plain(body))]);
                   },
                 );
            };
          },
        ),
      ]
    | Update(path) => [SendRequest(Load(path))]
    | Refine(spec) => [
        WithInstance(
          instance => {
            open Response.Specification;
            let payload = Handler.Spec.getPayload(spec, instance);
            resolve([SendRequest(Refine(spec.id, payload))]);
          },
        ),
      ];
};
