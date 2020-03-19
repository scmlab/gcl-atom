open Belt;
open Types.Command;
open Task__Types;

// from Editor Command to Tasks
let dispatch =
  fun
  | Toggle => [
      WithState(
        state =>
          if (state.State.loaded) {
            State.destroy(state);
            Promise.resolved([]);
          } else {
            state.loaded = true;
            state.view.setActivation(true) |> ignore;

            switch (state.connection) {
            | Some(_) => Promise.resolved([])
            | None =>
              state
              ->State.establishConnection
              ->Promise.map(
                  fun
                  | Ok(_) => [DispatchCommand(Save)]
                  | Error(error) => {
                      let (header, body) = State.Error.toString(error);
                      [Display(Error(header), Plain(body))];
                    },
                )
            };
          },
      ),
    ]
  | Save => [
      WithState(
        state => {
          state.decorations->Array.forEach(Atom.Decoration.destroy);
          state.editor
          ->Atom.TextEditor.save
          ->Promise.Js.fromBsPromise
          ->Promise.Js.catch(_ => Promise.resolved())
          ->Promise.map(() => {
              let filepath = Atom.TextEditor.getPath(state.editor);
              switch (filepath) {
              | Some(path) => [SendRequest(Load(path))]
              | None => [
                  Display(
                    Error("Cannot read filepath"),
                    Plain("Please save the file first"),
                  ),
                ]
              };
            });
        },
      ),
    ]
  | Refine => [
      DispatchCommand(Save),
      WithState(
        state =>
          Spec.fromCursorPosition(state)
          ->Option.mapWithDefault(
              Promise.resolved([]),
              spec => {
                let payload = Spec.getPayload(spec, state);
                Promise.resolved([SendRequest(Refine(spec.id, payload))]);
              },
            ),
      ),
    ]
  | InsertAssertion => [
      DispatchCommand(Save),
      WithState(
        state => {
          let cursor = Atom.TextEditor.getCursorBufferPosition(state.editor);
          open Base.Pos;
          let Pos(_, line, _) = Base.Pos.fromPoint("whatever", cursor);
          Promise.resolved([SendRequest(InsertAssertion(line))]);
        },
      ),
    ]
  | Debug => [SendRequest(Debug)];