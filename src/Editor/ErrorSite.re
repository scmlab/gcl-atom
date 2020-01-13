open Rebase;

type t =
  | Global(Atom.Range.t)
  | Local(Atom.Range.t, int);

open Json.Decode;
open Decoder;

let decode: decoder(t) =
  sum(
    fun
    | "Global" => Contents(json => Global(json |> range))
    | "Local" =>
      Contents(pair(range, int) |> map(((r, i)) => Local(r, i)))
    | tag => raise(DecodeError("Unknown constructor: " ++ tag)),
  );

let toRange = (site, specifications) => {
  switch (site) {
  | Global(range) => range
  | Local(range, i) =>
    open Atom.Range;
    open Specification;
    let specs = specifications |> Array.filter(spec => spec.id == i);

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
