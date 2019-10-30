open Rebase;

open Response;

let handle = (instance: Type.instance) =>
  fun
  | OK => {
      instance.view.setHeader(AllGood) |> ignore;
      instance.view.setBody(Nothing) |> ignore;
    }
  | ParseError(pairs) => {
      let decorate = ((pos, msg)) => {
        instance.view.setHeader(Error("Parse Error")) |> ignore;
        instance.view.setBody(Plain(msg)) |> ignore;
        open Atom;
        let range = Range.make(pos, pos);
        let marker = instance.editor |> TextEditor.markBufferRange(range);
        let option =
          TextEditor.decorateMarkerOptions(
            ~type_="line",
            ~class_="marker-error",
            (),
          );
        instance.editor |> Atom.TextEditor.decorateMarker(marker, option);
      };
      let decorations = pairs |> Array.map(decorate);
      instance.decorations = Array.concat(instance.decorations, decorations);

      ();
    }
  | UnknownResponse(json) => {
      instance.view.setHeader(Error("Panic: unknown response from GCL"))
      |> ignore;
      instance.view.setBody(Plain(Js.Json.stringify(json))) |> ignore;
    };
