open Rebase;
open Base;
open GCL.Error.Site;
open GCL.Response.Specification;

let toLoc = (site, specifications) => {
  switch (site) {
  | Global(loc) => loc
  | Local(loc, i) =>
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
