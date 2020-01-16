type t('a) = {
  emitter: Nd.Events.t,
  emit: 'a => unit,
  once: unit => Promise.t('a),
  on: ('a => unit, unit) => unit,
  destroy: unit => unit,
};

let make = () => {
  let emitter = Nd.Events.make();
  {
    emitter,
    emit: x => emitter |> Nd.Events.emit("data", x) |> ignore,
    once: () => {
      let (promise, resolve) = Promise.pending();
      emitter |> Nd.Events.once("data", resolve) |> ignore;
      promise;
    },
    on: callback => {
      emitter |> Nd.Events.on("data", callback) |> ignore;
      () => emitter |> Nd.Events.removeListener("data", callback) |> ignore;
    },
    destroy: () => Nd.Events.removeAllListeners(emitter) |> ignore,
  };
};
