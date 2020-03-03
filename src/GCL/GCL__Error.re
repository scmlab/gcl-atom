open Rebase;
open Base;
open Syntax;

module Site = {
  open Rebase;

  type t =
    | Global(loc)
    | Local(loc, int);

  open Json.Decode;
  open Util.Decode;

  let decode: decoder(t) =
    sum(
      fun
      | "Global" => Contents(json => Global(json |> Loc.decode))
      | "Local" =>
        Contents(pair(Loc.decode, int) |> map(((r, i)) => Local(r, i)))
      | tag => raise(DecodeError("Unknown constructor: " ++ tag)),
    );

  let toString = site => {
    switch (site) {
    | Global(loc) => "at " ++ Loc.toString(loc)
    | Local(loc, i) =>
      "at " ++ Loc.toString(loc) ++ " in #" ++ string_of_int(i)
    };
  };
};

module TypeError = {
  type t =
    | NotInScope(string)
    | UnifyFailed(Type.t, Type.t)
    | RecursiveType(int, Type.t)
    | NotFunction(Type.t);

  open Json.Decode;
  open Util.Decode;
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
          pair(Type.decode, Loc.decode) |> map(((t, _)) => NotFunction(t)),
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

  open Json.Decode;
  open Util.Decode;
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
};

type kind =
  | LexicalError
  | SyntacticError(array(string))
  | StructError(StructError.t)
  | TypeError(TypeError.t)
  | CannotReadFile(string)
  | NotLoaded;

open Json.Decode;
open Util.Decode;
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
  | Error(Site.t, kind);

let decode: decoder(t) =
  pair(Site.decode, decodeKind) |> map(((site, kind)) => Error(site, kind));
