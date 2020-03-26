module Impl: Guacamole.Editor.Interface = {
  open Atom;

  let setGCLPath = value => {
    Config.set("gcl-atom.path", value) |> ignore;
    Promise.resolved();
  };
  let getGCLPath = () => Config.get("gcl-atom.path");
};

include Impl;