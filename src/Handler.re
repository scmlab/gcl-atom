open Rebase;

open Response;

let handle = (instance: Type.instance) =>
  fun
  | OK => {
      instance.view.setHeader("All good") |> ignore;
      instance.view.setBody("") |> ignore;
    }
  | ParseError(pairs) => {
      let decorate = ((pos, body)) => {
        instance.view.setHeader("Parse error") |> ignore;
        instance.view.setBody(body) |> ignore;
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
    }; // let marker = Atom.DisplayMarker.
