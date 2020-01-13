open Json.Decode;
open Decoder;
open Rebase;

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
          Contents(pair(string, range) |> map(((s, _)) => NotInScope(s)))
        | "UnifyFailed" =>
          Contents(
            tuple3(Type.decode, Type.decode, range)
            |> map(((s, t, _)) => UnifyFailed(s, t)),
          )
        | "RecursiveType" =>
          Contents(
            tuple3(int, Type.decode, range)
            |> map(((s, t, _)) => RecursiveType(s, t)),
          )
        | "NotFunction" =>
          Contents(
            pair(Type.decode, range) |> map(((t, _)) => NotFunction(t)),
          )
        | tag => raise(DecodeError("Unknown constructor: " ++ tag)),
      );
  };

  module ConvertError = {
    type t =
      | MissingBound
      | MissingAssertion
      | ExcessBound
      | MissingPostcondition
      | DigHole
      | Panic(string);

    let decode: decoder(t) =
      sum(
        fun
        | "MissingBound" => Contents(_ => MissingBound)
        | "MissingAssertion" => Contents(_ => MissingAssertion)
        | "ExcessBound" => Contents(_ => ExcessBound)
        | "MissingPostcondition" => TagOnly(_ => MissingPostcondition)
        | "DigHole" => Contents(_ => DigHole)
        | "Panic" => Contents(json => Panic(json |> string))
        | tag => raise(DecodeError("Unknown constructor: " ++ tag)),
      );
  };

  type kind =
    | LexicalError
    | SyntacticError(string)
    | ConvertError(ConvertError.t)
    | TypeError(TypeError.t);

  let decodeKind: decoder(kind) =
    sum(
      fun
      | "LexicalError" => TagOnly(_ => LexicalError)
      | "SyntacticError" =>
        Contents(
          pair(range, string) |> map(((_, msg)) => SyntacticError(msg)),
        )
      | "ConvertError" =>
        Contents(json => ConvertError(json |> ConvertError.decode))
      | "TypeError" => Contents(json => TypeError(json |> TypeError.decode))
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
    | SyntacticError(message) => [
        AddDecorations(Decoration.markSite(site)),
        Display(Error("Parse Error"), Plain(message)),
      ]
    | ConvertError(MissingBound) => [
        AddDecorations(Decoration.markSite(site)),
        Display(
          Error("Bound Missing"),
          Plain(
            "Bound missing at the end of the assertion before the DO construct \" , bnd : ... }\"",
          ),
        ),
      ]
    | ConvertError(MissingAssertion) => [
        AddDecorations(Decoration.markSite(site)),
        Display(
          Error("Assertion Missing"),
          Plain("Assertion before the DO construct is missing"),
        ),
      ]
    | ConvertError(ExcessBound) => [
        AddDecorations(Decoration.markSite(site)),
        Display(
          Error("Excess Bound"),
          Plain("Unnecessary bound annotation at this assertion"),
        ),
      ]
    | ConvertError(MissingPostcondition) => [
        Display(
          Error("Postcondition Missing"),
          Plain("The last statement of the program should be an assertion"),
        ),
      ]
    | ConvertError(DigHole) => [
        WithInstance(
          instance => {
            let%P _ = instance |> Spec.digHole(site);
            switch (instance.history) {
            | Some(Types.Command.Refine(_)) =>
              Promise.resolved([DispatchLocal(Save), DispatchLocal(Refine)])
            | _ => Promise.resolved([DispatchLocal(Save)])
            };
          },
        ),
      ]
    | ConvertError(Panic(message)) => [
        Display(
          Error("Panic"),
          Plain(
            "This should not have happened, please report this issue\n"
            ++ message,
          ),
        ),
      ]
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
    };
  };
};

type t =
  | Error(array(Error.t))
  | OK(array(Body.ProofObligation.t), array(Specification.t))
  | Resolve(int)
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
