module Origin = {
  open Base;
  type t =
    | AtAbort(loc)
    | AtSkip(loc)
    | AtSpec(loc)
    | AtAssignment(loc)
    | AtAssertion(loc)
    | AtLoopInvariant(loc)
    | AtIf(loc)
    | AtLoop(loc)
    | AtTermination(loc)
    | AtBoundDecrement(loc);

  open Util.Decode;
  open! Json.Decode;
  let decode: decoder(t) =
    sum(
      fun
      | "AtAbort" => Contents(Loc.decode |> map(x => AtAbort(x)))
      | "AtSkip" => Contents(Loc.decode |> map(x => AtSkip(x)))
      | "AtSpec" => Contents(Loc.decode |> map(x => AtSpec(x)))
      | "AtAssignment" => Contents(Loc.decode |> map(x => AtAssignment(x)))
      | "AtAssertion" => Contents(Loc.decode |> map(x => AtAssertion(x)))
      | "AtLoopInvariant" =>
        Contents(Loc.decode |> map(x => AtLoopInvariant(x)))
      | "AtIf" => Contents(Loc.decode |> map(x => AtIf(x)))
      | "AtLoop" => Contents(Loc.decode |> map(x => AtLoop(x)))
      | "AtTermination" => Contents(Loc.decode |> map(x => AtTermination(x)))
      | "AtBoundDecrement" =>
        Contents(Loc.decode |> map(x => AtBoundDecrement(x)))
      | tag => raise(DecodeError("Unknown constructor: " ++ tag)),
    );

  let toString =
    fun
    | AtAbort(_) => "Abort"
    | AtSkip(_) => "Skip"
    | AtSpec(_) => "Spec"
    | AtAssignment(_) => "Assignment"
    | AtAssertion(_) => "Assertion"
    | AtLoopInvariant(_) => "Loop Invariant"
    | AtIf(_) => "Conditional"
    | AtLoop(_) => "Loop"
    | AtTermination(_) => "Termination"
    | AtBoundDecrement(_) => "Bound Decrement";

  let locOf =
    fun
    | AtAbort(loc) => loc
    | AtSkip(loc) => loc
    | AtSpec(loc) => loc
    | AtAssignment(loc) => loc
    | AtAssertion(loc) => loc
    | AtLoopInvariant(loc) => loc
    | AtIf(loc) => loc
    | AtLoop(loc) => loc
    | AtTermination(loc) => loc
    | AtBoundDecrement(loc) => loc;
};

module ProofObligation = {
  type t =
    | ProofObligation(int, Syntax.Pred.t, Syntax.Pred.t, Origin.t);

  open Json.Decode;
  let decode: decoder(t) =
    tuple4(int, Syntax.Pred.decode, Syntax.Pred.decode, Origin.decode)
    |> map(((i, p, q, o)) => ProofObligation(i, p, q, o));
};

module Specification = {
  open Base;
  type t = {
    id: int,
    pre: Syntax.Pred.t,
    post: Syntax.Pred.t,
    loc,
  };

  open Json.Decode;
  let decode: decoder(t) =
    json => {
      id: json |> field("specID", int),
      pre: json |> field("specPreCond", Syntax.Pred.decode),
      post: json |> field("specPostCond", Syntax.Pred.decode),
      loc: json |> field("specLoc", Loc.decode),
    };
};

type t =
  | Error(array(GCL__Error.t))
  | OK(array(ProofObligation.t), array(Specification.t))
  | Resolve(int)
  | InsertAssertion(Syntax.Expr.t)
  | UnknownResponse(Js.Json.t);

open Json.Decode;
open Util.Decode;
let decode: decoder(t) =
  sum(
    fun
    | "Error" =>
      Contents(array(GCL__Error.decode) |> map(errors => Error(errors)))
    | "OK" =>
      Contents(
        pair(array(ProofObligation.decode), array(Specification.decode))
        |> map(((obs, specs)) => OK(obs, specs)),
      )
    | "Insert" =>
      Contents(Syntax.Expr.decode |> map(i => InsertAssertion(i)))
    | "Resolve" => Contents(int |> map(i => Resolve(i)))
    | tag => raise(DecodeError("Unknown constructor: " ++ tag)),
  );
