open Json.Decode;
open Decoder;
open Rebase;

module Site = {
  type t =
    | Global(Atom.Range.t)
    | Local(Atom.Range.t, int);

  let decode: decoder(t) =
    sum(
      fun
      | "Global" => Contents(json => Global(json |> range))
      | "Local" =>
        Contents(pair(range, int) |> map(((r, i)) => Local(r, i)))
      | tag => raise(DecodeError("Unknown constructor: " ++ tag)),
    );

  let toRange = (site, instance) => {
    switch (site) {
    | Global(range) => range
    | Local(range, i) =>
      open Atom.Range;
      open Specification;
      open Types.Instance;
      let specs =
        instance.specifications |> Array.filter(spec => spec.id == i);

      specs[0]
      |> Option.mapOr(
           spec =>
             range
             |> translate(start(spec.range), start(spec.range))
             // down by 1 line
             |> translate(Atom.Point.make(1, 0), Atom.Point.make(1, 0)),
           range,
         );
    };
  };

  let toString = site => {
    let rangeToString = range => {
      Atom.Range.(
        Atom.Point.(
          string_of_int(row(start(range)))
          ++ ":"
          ++ string_of_int(column(start(range)))
          ++ "-"
          ++ string_of_int(row(end_(range)))
          ++ ":"
          ++ string_of_int(column(end_(range)))
        )
      );
    };
    switch (site) {
    | Global(range) => "at " ++ rangeToString(range)
    | Local(range, i) =>
      "at " ++ rangeToString(range) ++ " in #" ++ string_of_int(i)
    };
  };
};

module Decoration = {
  open Types.Instance;
  // Markers

  let mark = (type_, class_, range, instance) => {
    open Atom;
    let marker = instance.editor |> TextEditor.markBufferRange(range);
    let option = TextEditor.decorateMarkerOptions(~type_, ~class_, ());
    let decoration =
      instance.editor |> Atom.TextEditor.decorateMarker(marker, option);
    instance.decorations =
      Array.concat(instance.decorations, [|decoration|]);
  };

  let markLineSpecSoft = mark("highlight", "highlight-spec-soft");
  let markLineSpecHard = mark("highlight", "highlight-spec-hard");

  let overlay =
      (
        text,
        class_,
        tail: bool,
        translation: (int, int),
        range: Atom.Range.t,
        instance,
      ) => {
    open Atom;
    open Webapi.Dom;
    // create an element for the overlay
    let element = Webapi.Dom.document |> Document.createElement("div");
    Element.setInnerHTML(element, text);
    element |> Element.classList |> DomTokenList.add(class_);

    // adjusting the position of the overlay
    // setStyle is not supported by Reason Webapi for the moment, so we use setAttribute instead

    let (y, x) = translation;
    let left = x;
    let top = float_of_int(y - 1) *. 1.5;

    element
    |> Element.setAttribute(
         "style",
         "left: "
         ++ string_of_int(left)
         ++ "ex; top: "
         ++ Js.Float.toString(top)
         ++ "em",
       );

    // decorate
    let marker = instance.editor |> TextEditor.markBufferRange(range);
    let option =
      TextEditor.decorateMarkerOptions(
        ~type_="overlay",
        ~position=tail ? "tail" : "head",
        ~item=Element.unsafeAsHtmlElement(element),
        (),
      );
    let decoration =
      instance.editor |> TextEditor.decorateMarker(marker, option);
    instance.decorations =
      Array.concat(instance.decorations, [|decoration|]);
  };

  let overlaySpec = (text, range: Atom.Range.t, instance) => {
    overlay(text, "overlay-spec-text", false, (0, 1), range, instance);
  };

  let overlayError = (range: Atom.Range.t, instance) => {
    let length =
      instance.editor
      |> Atom.TextEditor.getTextInBufferRange(range)
      |> Js.String.length;
    let text = Js.String.repeat(length, "&nbsp;");
    overlay(text, "overlay-error", true, (0, 0), range, instance);
  };

  let markSpec = (spec: Specification.t, instance) => {
    open Specification;
    open Atom;
    // let {hardness, pre, post, _, range} = spec;

    let start = Range.start(spec.range);
    let start = Range.make(start, Point.translate(Point.make(0, 2), start));
    let end_ = Range.end_(spec.range);
    let end_ = Range.make(Point.translate(Point.make(0, -2), end_), end_);

    switch (spec.hardness) {
    | Hard => markLineSpecHard(start, instance)
    | Soft => markLineSpecSoft(start, instance)
    };

    let trim = s =>
      if (String.length(s) > 77) {
        String.sub(~from=0, ~length=73, s) ++ " ...";
      } else {
        s;
      };

    let pre = trim(Expr.toString(spec.pre));
    let post = trim(Expr.toString(spec.post));
    overlaySpec(pre, start, instance);
    overlaySpec(post, end_, instance);
    markLineSpecSoft(end_, instance);
  };

  let markSite = (site, instance) => {
    let range = instance |> Site.toRange(site);
    overlayError(range, instance);
    mark("line-number", "line-number-error", range, instance);
    Promise.resolved([]);
  };

  // destroy all decorations
  let destroyAll = instance =>
    instance.decorations |> Array.forEach(Atom.Decoration.destroy);
};

module Spec = {
  open Types.Instance;
  open Specification;

  let fromCursorPosition = instance => {
    open Atom;

    let cursor = instance.editor |> Atom.TextEditor.getCursorBufferPosition;
    // find the smallest hole containing the cursor
    let smallestHole = ref(None);
    instance.specifications
    |> Array.filter(spec => Range.containsPoint(cursor, spec.range))
    |> Array.forEach(spec =>
         switch (smallestHole^) {
         | None => smallestHole := Some(spec)
         | Some(spec') =>
           if (Range.containsRange(spec.range, spec'.range)) {
             smallestHole := Some(spec);
           }
         }
       );

    smallestHole^;
  };

  let getPayloadRange = (spec, instance) => {
    open Atom;

    // return the text in the targeted hole
    let start = Point.translate(Range.start(spec.range), Point.make(1, 0));
    let end_ =
      instance.editor
      |> TextEditor.getBuffer
      |> TextBuffer.rangeForRow(
           Point.row(Range.end_(spec.range)) - 1,
           true,
         )
      |> Range.end_;
    Range.make(start, end_);
  };

  let getPayload = (spec, instance) => {
    open Atom;
    // return the text in the targeted hole
    let innerRange = getPayloadRange(spec, instance);
    instance.editor
    |> TextEditor.getBuffer
    |> TextBuffer.getTextInRange(innerRange);
  };

  let resolve = (i, instance) => {
    open Atom;

    let specs = instance.specifications |> Array.filter(spec => spec.id == i);
    specs[0]
    |> Option.forEach(spec => {
         let payload = getPayload(spec, instance);
         let start = Range.start(spec.range);

         instance.editor
         |> TextEditor.getBuffer
         |> TextBuffer.delete(spec.range)
         |> ignore;

         instance.editor
         |> TextEditor.getBuffer
         |> TextBuffer.insert(start, Js.String.trim(payload))
         |> ignore;
       });
    Promise.resolved();
  };

  // rewrite "?" to "{!!}"
  let digHole = (site, instance) => {
    let range = instance |> Site.toRange(site);
    open Atom;
    let start = Range.start(range);
    // add indentation to the hole
    let indent = Js.String.repeat(Point.column(start), " ");
    let holeText = "{!\n" ++ indent ++ "\n" ++ indent ++ "!}";
    let holeRange =
      Range.make(start, Point.translate(start, Point.make(0, 1)));
    instance.editor
    |> TextEditor.setTextInBufferRange(holeRange, holeText)
    |> ignore;
    // set the cursor inside the hole
    let cursorPos = Point.translate(start, Point.make(1, 0));
    instance.editor |> TextEditor.setCursorBufferPosition(cursorPos);
    Promise.resolved();
  };
};

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
    | Error(Site.t, kind);

  let decode: decoder(t) =
    pair(Site.decode, decodeKind)
    |> map(((site, kind)) => Error(site, kind));

  let handle = error => {
    let Error(site, kind) = error;
    open Types.Task;
    ();
    switch (kind) {
    | LexicalError => [
        WithInstance(Decoration.markSite(site)),
        Display(Error("Lexical Error"), Plain(Site.toString(site))),
      ]
    | SyntacticError(message) => [
        WithInstance(Decoration.markSite(site)),
        Display(Error("Parse Error"), Plain(message)),
      ]
    | ConvertError(MissingBound) => [
        WithInstance(Decoration.markSite(site)),
        Display(
          Error("Bound Missing"),
          Plain(
            "Bound missing at the end of the assertion before the DO construct \" , bnd : ... }\"",
          ),
        ),
      ]
    | ConvertError(MissingAssertion) => [
        WithInstance(Decoration.markSite(site)),
        Display(
          Error("Assertion Missing"),
          Plain("Assertion before the DO construct is missing"),
        ),
      ]
    | ConvertError(ExcessBound) => [
        WithInstance(Decoration.markSite(site)),
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
            Js.log("dig!");
            let%P _ = instance |> Spec.digHole(site);

            switch (instance.history) {
            | Some(Types.Command.Refine(_)) =>
              Js.log("!!");
              Promise.resolved([
                DispatchLocal(Save),
                DispatchLocal(Refine),
              ]);
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
        WithInstance(Decoration.markSite(site)),
        Display(
          Error("Type Error"),
          Plain("The definition " ++ name ++ " is not in scope"),
        ),
      ]
    | TypeError(UnifyFailed(s, t)) => [
        WithInstance(Decoration.markSite(site)),
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
        WithInstance(Decoration.markSite(site)),
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
        WithInstance(Decoration.markSite(site)),
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
          WithInstance(
            instance => {
              specifications
              |> Array.forEach(Fn.flip(Decoration.markSpec, instance));
              instance.specifications = specifications;
              Promise.resolved([]);
            },
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
