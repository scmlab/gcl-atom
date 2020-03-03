open Rebase;

open! GCL__Response;

// from GCL response to Task
let handle = (response): list(Types.Task.t) => {
  Types.Command.(
    Types.Task.(
      switch (response) {
      | Error(errors) =>
        errors |> Array.map(Error.handle) |> List.fromArray |> Js.List.flatten
      | OK(obligations, specifications) => [
          SetSpecifications(specifications),
          AddDecorations(
            (specifications, editor) =>
              specifications
              |> Array.map(Fn.flip(Decoration.markSpec, editor))
              |> Array.map(List.fromArray)
              |> List.fromArray
              |> Js.List.flatten
              |> Array.fromList,
          ),
          Display(
            Plain("Proof Obligations"),
            ProofObligations(obligations),
          ),
        ]
      | Resolve(i) => [
          WithInstance(
            instance => {
              let%P _ = Spec.resolve(i, instance);
              Promise.resolved([DispatchLocal(Save)]);
            },
          ),
        ]
      | InsertAssertion(i) => [
          WithInstance(
            _instance => {
              Js.log(i);
              // let%P _ = Spec.resolve(i, instance);
              Promise.resolved([]);
            },
          ),
        ]
      | UnknownResponse(json) => [
          Display(
            Error("Panic: unknown response from GCL"),
            Plain(Js.Json.stringify(json)),
          ),
        ]
      }
    )
  );
};
