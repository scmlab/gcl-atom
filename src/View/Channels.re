type t = {
  updateConnection:
    Channel.t((Connection.t, option(Connection.Error.t)), unit, unit),
  setActivation: Channel.t(bool, unit, unit),
  setHeader: Channel.t(string, unit, unit),
  setBody: Channel.t(string, unit, unit),
};

let make = () => {
  updateConnection: Channel.make(),
  setActivation: Channel.make(),
  setHeader: Channel.make(),
  setBody: Channel.make(),
};
