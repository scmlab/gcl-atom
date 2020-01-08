/* open Async; */
open! Rebase;

module Listener = {
  type t('a) = {
    resolve: 'a => unit,
    id: int,
  };
  let make = (resolve, id): t('a) => {resolve, id};
  // Listener(B) -> Listener(A)
  let contraMap = (f: 'a => 'b, listener: t('b)): t('a) => {
    resolve: (event: 'a) => listener.resolve(f(event)),
    id: listener.id,
  };
};

type t('a) = {
  counter: ref(int),
  listeners: Js.Dict.t(Listener.t('a)),
};

let make = () => {counter: ref(0), listeners: Js.Dict.empty()};

let removeListener = (_id: int, _self: t('a)) => {
  %raw
  "delete _self[1][String(_id)]";
};

let removeListener' = (_id: string, _self: t('a)) => {
  %raw
  "delete _self[1][_id]";
};
let removeAllListeners = (self: t('a)) => {
  self.listeners
  |> Js.Dict.keys
  |> Array.forEach(id => removeListener'(id, self))
  |> ignore;
};
let destroy = removeAllListeners;

let listen = (callback: 'a => unit, self: t('a)): (unit => unit) => {
  /* get and update the ID counter  */
  let id: int = self.counter^ + 1;
  self.counter := id;
  /* store the callback */
  let listener = Listener.make(callback, id);
  Js.Dict.set(self.listeners, string_of_int(id), listener);

  /* returns the destructor */
  let destructor = () => {
    removeListener(id, self);
  };
  destructor;
};

let destroyWhen =
    (trigger: (unit => unit) => unit, destructor: unit => unit): unit => {
  trigger(destructor);
};

/* alias of `listen` */
let on = listen;

let contraMap = (f: 'a => 'b, x: t('b)): t('a) => {
  counter: x.counter,
  listeners: x.listeners |> Js.Dict.map((. l) => l |> Listener.contraMap(f)),
};

let onOk: ('a => unit, t(result('a, 'e)), unit) => unit =
  callback =>
    on(
      fun
      | Ok(a) => callback(a)
      | Error(_) => (),
    );

let onError: ('e => unit, t(result('a, 'e)), unit) => unit =
  callback =>
    on(
      fun
      | Ok(_) => ()
      | Error(e) => callback(e),
    );

let once = (self: t('a)): Promise.t('a) => {
  // get and update the ID counter
  let id: int = self.counter^ + 1;
  self.counter := id;
  // makes a new promise
  let (promise, resolve) = Promise.pending();

  let callback = a => {
    resolve(a);
    removeListener(id, self);
  };

  let listener = Listener.make(callback, id);
  Js.Dict.set(self.listeners, string_of_int(id), listener);

  promise;
};

/* generic emit */
let emit = (x: 'a, self: t('a)): unit => {
  self.listeners
  |> Js.Dict.values
  |> Array.forEach((listener: Listener.t('a)) => listener.resolve(x));
};
/* successful emit */
let emitOk = (x: 'a, self: t(result('a, 'e))): unit => emit(Ok(x), self);

/* failed emit */
let emitError = (e: 'e, self: t(result('a, 'e))): unit =>
  emit(Error(e), self);

/* from |> pipe(to_) */
let pipe =
    (to_: t(result('a, 'e)), from: t(result('a, 'e))): (unit => unit) =>
  from
  |> on(
       fun
       | Ok(ok) => to_ |> emitOk(ok)
       | Error(err) => to_ |> emitError(err),
     );
