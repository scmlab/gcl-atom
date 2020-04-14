// open Belt;

// open! Guacamole.GCL.Response;
// open Task__Types;
// open Types.Command;

// // from GCL response to Task
// let handle = (response): list(Task__Types.t) => {
//   switch (response) {
//   | Error(errors) =>
//     errors->Array.map(Task__Error.handle)->List.fromArray->Js.List.flatten
//   | OK(obligations, specifications) => [
//       SetSpecifications(specifications),
//       AddDecorations(
//         (specifications, editor) =>
//           specifications
//           ->Array.map(Decoration.markSpec(editor))
//           ->Array.map(List.fromArray)
//           ->List.fromArray
//           ->Js.List.flatten
//           ->List.toArray,
//       ),
//       Display(Plain("Proof Obligations"), ProofObligations(obligations)),
//     ]
//   | Resolve(i) => [
//       WithState(
//         state => {
//           let%P _ = Spec.resolve(i, state);
//           Promise.resolved([DispatchCommand(Save)]);
//         },
//       ),
//     ]
//   | InsertAssertion(i, expr) => [
//       WithState(
//         state => {
//           Spec.insert(i, expr, state);
//           Promise.resolved([DispatchCommand(Save)]);
//         },
//       ),
//     ]
//   | UnknownResponse(json) => [
//       Display(
//         Error("Panic: unknown response from GCL"),
//         Plain(Js.Json.stringify(json)),
//       ),
//     ]
//   };
// };

();