open Base;

type event =
  | MouseOver(loc)
  | MouseLeave(loc);

let emitter: Event.t(event) = Event.make();
let eventContext = React.createContext(emitter);

module Provider = {
  let makeProps = (~value, ~children, ()) => {
    "value": value,
    "children": children,
  };

  let make = React.Context.provider(eventContext);
};

[@react.component]
let make = (~loc, ~children) => {
  let link = React.useContext(eventContext);
  let onMouseOver = _ => link.emit(MouseOver(loc));
  let onMouseLeave = _ => link.emit(MouseLeave(loc));
  <div className="expr-link" onMouseOver onMouseLeave> children </div>;
};
