open Rebase;

open Response;

let markLineError = (range, instance: Type.instance) => {
  open Atom;
  let marker = instance.editor |> TextEditor.markBufferRange(range);
  let option =
    TextEditor.decorateMarkerOptions(
      ~type_="line",
      ~class_="marker-error",
      (),
    );
  let decoration =
    instance.editor |> Atom.TextEditor.decorateMarker(marker, option);

  instance.decorations = Array.concat(instance.decorations, [|decoration|]);
};

let markRangeError = (range, instance: Type.instance) => {
  open Atom;
  let marker = instance.editor |> TextEditor.markBufferRange(range);
  let option =
    TextEditor.decorateMarkerOptions(
      ~type_="highlight",
      ~class_="marker-error",
      (),
    );
  let decoration =
    instance.editor |> Atom.TextEditor.decorateMarker(marker, option);

  instance.decorations = Array.concat(instance.decorations, [|decoration|]);
};

let handle = (instance: Type.instance) =>
  fun
  | OK => {
      instance.view.setHeader(AllGood) |> ignore;
      instance.view.setBody(Nothing) |> ignore;
      Async.resolve();
    }
  | ParseError(errors) => {
      // TODO: reporting only the first error now
      switch (errors[0]) {
      | None =>
        instance.view.setHeader(AllGood) |> ignore;
        instance.view.setBody(Nothing) |> ignore;
        Async.resolve();
      | Some((pos, msg)) =>
        instance.view.setHeader(Error("Parse Error")) |> ignore;
        instance.view.setBody(Plain(msg)) |> ignore;

        let range = Atom.Range.make(pos, pos);
        instance |> markLineError(range);

        Async.resolve();
      };
    }
  | SyntaxError(MissingBound(range)) => {
      instance.view.setHeader(Error("Bound Missing")) |> ignore;
      instance.view.setBody(
        Plain(
          "Bound missing at the end of the assertion before the DO construct \" , bnd : ... }\"",
        ),
      )
      |> ignore;
      instance |> markRangeError(range);
      Async.resolve();
    }
  | SyntaxError(MissingAssertion(range)) => {
      instance.view.setHeader(Error("Assertion Missing")) |> ignore;
      instance.view.setBody(
        Plain("Assertion before the DO construct is missing"),
      )
      |> ignore;
      instance |> markRangeError(range);
      Async.resolve();
    }
  | SyntaxError(ExcessBound(range)) => {
      instance.view.setHeader(Error("Excess Bound")) |> ignore;
      instance.view.setBody(
        Plain("Unnecessary bound annotation at this assertion"),
      )
      |> ignore;
      instance |> markRangeError(range);
      Async.resolve();
    }
  | SyntaxError(MissingPostcondition) => {
      instance.view.setHeader(Error("Postcondition Missing")) |> ignore;
      instance.view.setBody(
        Plain("The last statement of the program should be an assertion"),
      )
      |> ignore;
      Async.resolve();
    }
  | SyntaxError(Panic(message)) => {
      instance.view.setHeader(Error("Panic")) |> ignore;
      instance.view.setBody(
        Plain(
          "This should not have happened, please report this issue\n"
          ++ message,
        ),
      )
      |> ignore;
      Async.resolve();
    }
  | ProofObligations(obligations) => {
      instance.view.setHeader(Plain("Proof Obligations")) |> ignore;
      instance.view.setBody(ProofObligations(obligations)) |> ignore;
      Async.resolve();
    }
  | UnknownResponse(json) => {
      instance.view.setHeader(Error("Panic: unknown response from GCL"))
      |> ignore;
      instance.view.setBody(Plain(Js.Json.stringify(json))) |> ignore;
      Async.resolve();
    };
