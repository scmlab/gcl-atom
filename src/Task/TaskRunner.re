open! Rebase;
open Task__Types;

// run the Tasks
let rec run = (instance: Instance.t, tasks: list(Task.t)): Promise.t(unit) => {
  let runTask = task =>
    switch (task) {
    | WithInstance(callback) =>
      callback(instance)->Promise.flatMap(run(instance))
    | AddDecorations(callback) =>
      Js.log("[ add decorations ]");
      instance.decorations =
        Array.concat(
          callback(instance.specifications, instance.editor),
          instance.decorations,
        );
      Promise.resolved();
    | SetSpecifications(specs) =>
      Js.log("[ set specifications ]");
      instance.specifications = specs;
      Promise.resolved();
    | DispatchCommand(command) =>
      Js.log2("[ dispatch command ]", command);
      Task.Command.dispatch(command) |> run(instance);
    | SendRequest(request) =>
      Js.log("[ send request ]");
      Instance.Connection_.sendRequest(request, instance)
      ->Promise.flatMap(
          fun
          | Error(error) => {
              let (header, body) = Instance.Error.toString(error);
              instance |> Instance.View.displayError(header, body);
              Promise.resolved();
            }
          | Ok(x) => x |> Task.Response.handle |> run(instance),
        );
    | Display(header, body) =>
      instance.view.setHeader(header) |> ignore;
      instance.view.setBody(body) |> ignore;
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
