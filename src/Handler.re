open Response;

let handle = (instance: Type.instance) =>
  fun
  | OK => ()
  | ParseError(pos, _msg) => {
      open Atom;
      let range = Range.make(pos, pos);
      Js.log(pos);
      Js.log(range);
      let marker = instance.editor |> TextEditor.markBufferRange(range);
      let option =
        TextEditor.decorateMarkerOptions(
          ~type_="line",
          ~class_="marker-error",
          (),
        );
      let _decoration =
        instance.editor |> Atom.TextEditor.decorateMarker(marker, option);
      ();
    }; // let marker = Atom.DisplayMarker.
