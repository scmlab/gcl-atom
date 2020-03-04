open Rebase;

open! Response;
open Task__Types;
open Types.Command;

// from GCL response to Task
let handle = (response): list(Task__Types.t) => {
  switch (response) {
  | Error(errors) =>
    errors
    |> Array.map(Task__Error.handle)
    |> List.fromArray
    |> Js.List.flatten
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
      Display(Plain("Proof Obligations"), ProofObligations(obligations)),
    ]
  | Resolve(i) => [
      WithInstance(
        instance => {
          let%P _ = Spec.resolve(i, instance);
          Promise.resolved([DispatchCommand(Save)]);
        },
      ),
    ]
  | InsertAssertion(i, expr) => [
      WithInstance(
        instance => {
          Spec.insert(i, expr, instance);
          Promise.resolved([DispatchCommand(Save)]);
        },
      ),
    ]
  | UnknownResponse(json) => [
      Display(
        Error("Panic: unknown response from GCL"),
        Plain(Js.Json.stringify(json)),
      ),
    ]
  };
};
