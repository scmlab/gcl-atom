open Rebase;

open Response;

let handle = (instance: Type.instance) =>
  fun
  | OK => ()
  | ParseError(pairs) => {
      let decorate = ((pos, _msg)) => {
        open Atom;
        let pos' = Point.translate(pos, Point.make(-1, 0));
        let range = Range.make(pos', pos');
        Js.log(pos);
        Js.log(range);
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
