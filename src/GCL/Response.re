open Json.Decode;
open Decoder;
open Rebase;
open Base;

module Error = {
  module TypeError = {
    type t =
      | NotInScope(string)
      | UnifyFailed(Type.t, Type.t)
      | RecursiveType(int, Type.t)
      | NotFunction(Type.t);

    let decode: decoder(t) =
      sum(
        fun
        | "NotInScope" =>
          Contents(
            pair(string, Loc.decode) |> map(((s, _)) => NotInScope(s)),
          )
        | "UnifyFailed" =>
          Contents(
            tuple3(Type.decode, Type.decode, Loc.decode)
            |> map(((s, t, _)) => UnifyFailed(s, t)),
          )
        | "RecursiveType" =>
          Contents(
            tuple3(int, Type.decode, Loc.decode)
            |> map(((s, t, _)) => RecursiveType(s, t)),
          )
        | "NotFunction" =>
          Contents(
            pair(Type.decode, Loc.decode)
            |> map(((t, _)) => NotFunction(t)),
          )
        | tag => raise(DecodeError("Unknown constructor: " ++ tag)),
      );
  };

  module StructError = {
    type t =
      | MissingBound
      | MissingAssertion
      | MissingLoopInvariant
      | ExcessBound
      | MissingPrecondition
      | MissingPostcondition
      | DigHole;

    let decode: decoder(t) =
      sum(
        fun
        | "MissingBound" => Contents(_ => MissingBound)
        | "MissingAssertion" => Contents(_ => MissingAssertion)
        | "MissingLoopInvariant" => Contents(_ => MissingLoopInvariant)
        | "ExcessBound" => Contents(_ => ExcessBound)
        | "MissingPrecondition" => Contents(_ => MissingPrecondition)
        | "MissingPostcondition" => Contents(_ => MissingPostcondition)
        | "DigHole" => Contents(_ => DigHole)
        | tag => raise(DecodeError("Unknown constructor: " ++ tag)),
      );

    let handle = site =>
      fun
      | MissingBound => [
          Types.Task.AddDecorations(Decoration.markSite(site)),
          Display(
            Error("Bound Missing"),
            Plain(
              "Bound missing at the end of the assertion before the DO construct \" , bnd : ... }\"",
            ),
          ),
        ]
      | MissingAssertion => [
          AddDecorations(Decoration.markSite(site)),
          Display(
            Error("Assertion Missing"),
            Plain("Assertion before the DO construct is missing"),
          ),
        ]
      | MissingLoopInvariant => [
          AddDecorations(Decoration.markSite(site)),
          Display(
            Error("Loop Invariant Missing"),
            Plain("Loop invariant before the DO construct is missing"),
          ),
        ]
      | ExcessBound => [
          AddDecorations(Decoration.markSite(site)),
          Display(
            Error("Excess Bound"),
            Plain("Unnecessary bound annotation at this assertion"),
          ),
        ]
      | MissingPrecondition => [
          Display(
            Error("Precondition Missing"),
            Plain(
              "The first statement of the program should be an assertion",
            ),
          ),
        ]
      | MissingPostcondition => [
          Display(
            Error("Postcondition Missing"),
            Plain("The last statement of the program should be an assertion"),
          ),
        ]
      | DigHole => [
          WithInstance(
            instance => {
              let%P _ = instance |> Spec.digHole(site);
              switch (instance.history) {
              | Some(Types.Command.Refine(_)) =>
                Promise.resolved([
                  Types.Task.DispatchLocal(Save),
                  DispatchLocal(Refine),
                ])
              | _ => Promise.resolved([Types.Task.DispatchLocal(Save)])
              };
            },
          ),
        ];
  };

  type kind =
    | LexicalError
    | SyntacticError(array(string))
    | StructError(StructError.t)
    | TypeError(TypeError.t)
    | CannotReadFile(string)
    | NotLoaded;

  let decodeKind: decoder(kind) =
    sum(
      fun
      | "LexicalError" => TagOnly(_ => LexicalError)
      | "SyntacticError" =>
        Contents(
          array(pair(Loc.decode, string))
          |> map(pairs => SyntacticError(pairs |> Array.map(snd))),
        )
      | "StructError2" =>
        Contents(json => StructError(json |> StructError.decode))
      | "TypeError" => Contents(json => TypeError(json |> TypeError.decode))
      | "CannotReadFile" => Contents(json => CannotReadFile(json |> string))
      | "NotLoaded" => TagOnly(_ => NotLoaded)
      | tag => raise(DecodeError("Unknown constructor: " ++ tag)),
    );

  type t =
    | Error(ErrorSite.t, kind);

  let decode: decoder(t) =
    pair(ErrorSite.decode, decodeKind)
    |> map(((site, kind)) => Error(site, kind));

  let handle = error => {
    let Error(site, kind) = error;
    open Types.Task;
    ();
    switch (kind) {
    | LexicalError => [
        AddDecorations(Decoration.markSite(site)),
        Display(Error("Lexical Error"), Plain(ErrorSite.toString(site))),
      ]
    | SyntacticError(messages) => [
        AddDecorations(Decoration.markSite(site)),
        Display(
          Error("Parse Error"),
          Plain(messages |> List.fromArray |> String.joinWith("\n")),
        ),
      ]
    | StructError(error) => StructError.handle(site, error)
    | TypeError(NotInScope(name)) => [
        AddDecorations(Decoration.markSite(site)),
        Display(
          Error("Type Error"),
          Plain("The definition " ++ name ++ " is not in scope"),
        ),
      ]
    | TypeError(UnifyFailed(s, t)) => [
        AddDecorations(Decoration.markSite(site)),
        Display(
          Error("Type Error"),
          Plain(
            "Cannot unify: "
            ++ Type.toString(s)
            ++ "\nwith        : "
            ++ Type.toString(t),
          ),
        ),
      ]
    | TypeError(RecursiveType(var, t)) => [
        AddDecorations(Decoration.markSite(site)),
        Display(
          Error("Type Error"),
          Plain(
            "Recursive type variable: "
            ++ Type.toString(Type.Var(var))
            ++ "\n"
            ++ "in type             : "
            ++ Type.toString(t),
          ),
        ),
      ]
    | TypeError(NotFunction(t)) => [
        AddDecorations(Decoration.markSite(site)),
        Display(
          Error("Type Error"),
          Plain(
            "The type " ++ Type.toString(t) ++ " is not a function type",
          ),
        ),
      ]
    | CannotReadFile(path) => [
        AddDecorations(Decoration.markSite(site)),
        Display(
          Error("Cannot Read File"),
          Plain("Cannot read file of path: " ++ path),
        ),
      ]
    | NotLoaded => [
        AddDecorations(Decoration.markSite(site)),
        Display(Error("Not Loaded"), Plain("Please load the file first")),
      ]
    };
  };
};

type t =
  | Error(array(Error.t))
  | OK(array(Body.ProofObligation.t), array(Specification.t))
  | Resolve(int)
  | InsertAssertion(Syntax.Expr.t)
  | UnknownResponse(Js.Json.t);

let decode: decoder(t) =
  sum(
    fun
    | "Error" =>
      Contents(array(Error.decode) |> map(errors => Error(errors)))
    | "OK" =>
      Contents(
        pair(
          array(Body.ProofObligation.decode),
          array(Specification.decode),
        )
        |> map(((obs, specs)) => OK(obs, specs)),
      )
    | "Insert" =>
      Contents(Syntax.Expr.decode |> map(i => InsertAssertion(i)))
    | "Resolve" => Contents(int |> map(i => Resolve(i)))
    | tag => raise(DecodeError("Unknown constructor: " ++ tag)),
  );

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
