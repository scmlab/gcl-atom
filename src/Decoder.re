open Json.Decode;

type fieldType('a) =
  | Contents(decoder('a))
  | TagOnly(decoder('a));

let sum = decoder =>
  field("tag", string)
  |> andThen(tag =>
       switch (decoder(tag)) {
       | Contents(d) => field("contents", d)
       | TagOnly(d) => d
       }
     );

let point: decoder(Atom.Point.t) =
  json =>
    Atom.Point.make(
      field("line", int, json) - 1,
      field("column", int, json) - 1,
    );

let range: decoder(Atom.Range.t) =
  sum(
    fun
    | "Loc" =>
      Contents(
        json => {
          let x = json |> field("start", point);
          let y = json |> field("end", point);
          Atom.Point.(
            Atom.Range.make(
              make(row(x), column(x)),
              make(row(y), column(y) + 1),
            )
          );
        },
      )
    | "NoLoc" =>
      TagOnly(
        _ => Atom.Range.make(Atom.Point.make(0, 0), Atom.Point.make(0, 0)),
      )
    | tag => raise(DecodeError("Unknown constructor: " ++ tag)),
  );

let maybe: decoder('a) => decoder(option('a)) =
  decoder =>
    sum(
      fun
      | "Just" => Contents(json => Some(decoder(json)))
      | _ => TagOnly(_ => None),
    );
