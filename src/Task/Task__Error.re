// open Guacamole.GCL.Syntax;

// open Task__Types;
// open! Guacamole.GCL.Response.Error;

// module StructError = {
//   open Guacamole.GCL.Response.Error.StructError;
//   let handle = site =>
//     fun
//     | MissingBound => [
//         AddDecorations(Decoration.markSite(site)),
//         Display(
//           Error("Bound Missing"),
//           Plain(
//             "Bound missing at the end of the assertion before the DO construct \" , bnd : ... }\"",
//           ),
//         ),
//       ]
//     | MissingAssertion => [
//         AddDecorations(Decoration.markSite(site)),
//         Display(
//           Error("Assertion Missing"),
//           Plain("Assertion before the DO construct is missing"),
//         ),
//       ]
//     | MissingLoopInvariant => [
//         AddDecorations(Decoration.markSite(site)),
//         Display(
//           Error("Loop Invariant Missing"),
//           Plain("Loop invariant before the DO construct is missing"),
//         ),
//       ]
//     | ExcessBound => [
//         AddDecorations(Decoration.markSite(site)),
//         Display(
//           Error("Excess Bound"),
//           Plain("Unnecessary bound annotation at this assertion"),
//         ),
//       ]
//     | MissingPrecondition => [
//         Display(
//           Error("Precondition Missing"),
//           Plain("The first statement of the program should be an assertion"),
//         ),
//       ]
//     | MissingPostcondition => [
//         Display(
//           Error("Postcondition Missing"),
//           Plain("The last statement of the program should be an assertion"),
//         ),
//       ]
//     | DigHole => [
//         WithState(
//           state => {
//             let%P _ = state |> Spec.digHole(site);
//             switch (state.history) {
//             | Some(Types.Request.Refine(_)) =>
//               Promise.resolved([
//                 DispatchCommand(Save),
//                 DispatchCommand(Refine),
//               ])
//             | _ => Promise.resolved([DispatchCommand(Save)])
//             };
//           },
//         ),
//       ];
// };

// let handle = error => {
//   let Error(site, kind) = error;
//   switch (kind) {
//   | LexicalError => [
//       AddDecorations(Decoration.markSite(site)),
//       Display(
//         Error("Lexical Error"),
//         Plain(Guacamole.GCL.Response.Error.Site.toString(site)),
//       ),
//     ]
//   | SyntacticError(messages) => [
//       AddDecorations(Decoration.markSite(site)),
//       Display(
//         Error("Parse Error"),
//         Plain(messages->Js.String.concatMany("\n")),
//       ),
//     ]
//   | StructError(error) => StructError.handle(site, error)
//   | TypeError(NotInScope(name)) => [
//       AddDecorations(Decoration.markSite(site)),
//       Display(
//         Error("Type Error"),
//         Plain("The definition " ++ name ++ " is not in scope"),
//       ),
//     ]
//   | TypeError(UnifyFailed(s, t)) => [
//       AddDecorations(Decoration.markSite(site)),
//       Display(
//         Error("Type Error"),
//         Plain(
//           "Cannot unify: "
//           ++ Type.toString(s)
//           ++ "\nwith        : "
//           ++ Type.toString(t),
//         ),
//       ),
//     ]
//   | TypeError(RecursiveType(var, t)) => [
//       AddDecorations(Decoration.markSite(site)),
//       Display(
//         Error("Type Error"),
//         Plain(
//           "Recursive type variable: "
//           ++ Type.toString(Type.Var(var))
//           ++ "\n"
//           ++ "in type             : "
//           ++ Type.toString(t),
//         ),
//       ),
//     ]
//   | TypeError(NotFunction(t)) => [
//       AddDecorations(Decoration.markSite(site)),
//       Display(
//         Error("Type Error"),
//         Plain("The type " ++ Type.toString(t) ++ " is not a function type"),
//       ),
//     ]
//   | CannotReadFile(path) => [
//       AddDecorations(Decoration.markSite(site)),
//       Display(
//         Error("Cannot Read File"),
//         Plain("Cannot read file of path: " ++ path),
//       ),
//     ]
//   | NotLoaded => [
//       AddDecorations(Decoration.markSite(site)),
//       Display(Error("Not Loaded"), Plain("Please load the file first")),
//     ]
//   };
// };

();