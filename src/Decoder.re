open Json.Decode;

type fieldType('a) =
  | Contents(decoder('a))
  | TagOnly(decoder('a));

let fields = decoder =>
  field("tag", string)
  |> andThen(tag =>
       switch (decoder(tag)) {
       | Contents(d) => field("contents", d)
       | TagOnly(d) => d
       }
     );
