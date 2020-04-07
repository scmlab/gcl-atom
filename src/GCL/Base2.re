module Pos = {
  open Guacamole.View.Pos;
  let toPoint =
    fun
    | Pos(_, line, column) => Atom.Point.make(line - 1, column - 1);
  let fromPoint = (filepath, point) => {
    Pos(filepath, Atom.Point.row(point) + 1, Atom.Point.column(point) + 1);
  };
};

module Loc = {
  open Guacamole.View.Loc;
  let toRange =
    fun
    | NoLoc => Atom.Range.make(Atom.Point.make(0, 0), Atom.Point.make(0, 0))
    | Loc(x, Pos(_, line, column)) =>
      Atom.Range.make(Pos.toPoint(x), Atom.Point.make(line - 1, column));
  let fromRange = (filepath, range) => {
    let start = Atom.Range.start(range);
    let end_ = Atom.Range.end_(range);
    Loc(
      Pos(
        filepath,
        Atom.Point.row(start) + 1,
        Atom.Point.column(start) + 1,
      ),
      Pos(filepath, Atom.Point.row(end_) + 1, Atom.Point.column(end_)),
    );
  };
};