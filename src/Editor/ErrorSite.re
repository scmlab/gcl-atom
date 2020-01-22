open Rebase;
open Base;

type t =
  | Global(loc)
  | Local(loc, int);

open Json.Decode;
open Decoder;

let decode: decoder(t) =
  sum(
    fun
    | "Global" => Contents(json => Global(json |> Loc.decode))
    | "Local" =>
      Contents(pair(Loc.decode, int) |> map(((r, i)) => Local(r, i)))
    | tag => raise(DecodeError("Unknown constructor: " ++ tag)),
  );

let toLoc = (site, specifications) => {
  switch (site) {
  | Global(loc) => loc
  | Local(loc, i) =>
    open Specification;
    let specs = specifications |> Array.filter(spec => spec.id == i);

    specs[0]
    |> Option.mapOr(
         spec =>
           spec.loc |> Loc.translate(loc) |> Loc.translateBy(1, 0, 1, 0),
         loc,
       );
  };
};

let toRange = (site, specifications) =>
  toLoc(site, specifications) |> Loc.toRange;

let toString = site => {
  switch (site) {
  | Global(loc) => "at " ++ Loc.toString(loc)
  | Local(loc, i) =>
    "at " ++ Loc.toString(loc) ++ " in #" ++ string_of_int(i)
  };
};
