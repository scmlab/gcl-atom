open React;

[@react.component]
let make =
    (
      ~header: Guacamole.View.Request.Header.t,
      ~editorType: Guacamole.Sig.editorType,
      ~mode: Guacamole.View.Response.mode,
      ~onChangeMode: Guacamole.View.Response.mode => unit,
    ) => {
  open! Guacamole.View.Request.Header;

  let onChange = _ => {
    open Guacamole.View.Response;
    let newMode =
      switch (mode) {
      | WP1 => WP2
      | WP2 => WP1
      };
    onChangeMode(newMode);
  };

  // display different type fo mode toggle base on the editor type
  let modeToggle =
    switch (editorType) {
    | Guacamole.Sig.VsCode =>
      <button className="gcl-mode-vscode" onClick=onChange>
        {string(
           switch (mode) {
           | WP1 => "WP"
           | WP2 => "WP2"
           },
         )}
      </button>
    | Atom =>
      <div className="gcl-mode-atom">
        <label className="input-label">
          <input
            className="input-toggle"
            type_="checkbox"
            checked={
              switch (mode) {
              | WP1 => false
              | WP2 => true
              }
            }
            onChange
          />
          {string("WP2")}
        </label>
      </div>
    };

  <h2 className="gcl-header">
    {switch (header) {
     | Loading => <div className="text-plain"> {string("Loading ...")} </div>
     | Plain(s) => <div> {string(s)} </div>
     | Error(s) => <div className="text-error"> {string(s)} </div>
     }}
    modeToggle
  </h2>;
};