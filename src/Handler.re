open Rebase;

open Response;

let handle = (instance: Type.instance) =>
  fun
  | OK => ()
  | ParseError(pairs) => {
      let decorate = ((pos, msg)) => {
        Js.log(pos);
        Js.log(msg);
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
