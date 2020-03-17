open Belt;
open Task__Types;

// run the Tasks
let rec run = (state: State.t, tasks: list(Task.t)): Promise.t(unit) => {
  let runTask = task =>
    switch (task) {
    | WithState(callback) => callback(state)->Promise.flatMap(run(state))
    | AddDecorations(callback) =>
      Js.log("[ add decorations ]");
      state.decorations =
        Array.concat(
          callback(state.specifications, state.editor),
          state.decorations,
        );
      Promise.resolved();
    | SetSpecifications(specs) =>
      Js.log("[ set specifications ]");
      state.specifications = specs;
      Promise.resolved();
    | DispatchCommand(command) =>
      Js.log2("[ dispatch command ]", command);
      Task.Command.dispatch(command) |> run(state);
    | SendRequest(request) =>
      Js.log("[ send request ]");
      State.sendRequest(request, state)
      ->Promise.flatMap(
          fun
          | Error(error) => {
              let (header, body) = State.Error.toString(error);
              state |> State.displayError(header, body);
              Promise.resolved();
            }
          | Ok(x) => x |> Task.Response.handle |> run(state),
        );
    | Display(header, body) =>
      state.view.setHeader(header) |> ignore;
      state.view.setBody(body) |> ignore;
      Promise.resolved();
    };

  let rec runEach =
    fun
    | [] => Promise.resolved()
    | [x, ...xs] => {
        let%P () = runTask(x);
        let%P () = runEach(xs);
        Promise.resolved();
      };
  runEach(tasks);
};