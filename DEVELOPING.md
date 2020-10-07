# Developing

## Overview

The Atom editor is essentially a web browser, plugins are developed using
web technologies:

* The program is written in [ReasonML](https://reasonml.github.io/), and then transpiled to JavaScript using [BuckleScript](https://bucklescript.github.io/en/)
* The interface is crafted using [React](https://reactjs.org/), a framework with a BuckleScript binding called [reason-react](https://reasonml.github.io/reason-react/).

## Interaction with the GCL backend

The plugin would send a request to the GCL backend, and expect the backend to
reply a response.
Requests and responses are encoded in JSON format.

## Setup

1. clone the repo
2. link it as a developing package
3. install dependencies
4. build the package

```
git clone git@github.com:scmlab/gcl-atom.git
apm develop gcl-atom <cloned directory>
cd <cloned directory> && yarn
yarn run build && apm rebuild
```

To open some file in the development mode:

```
atom -d file.gcl
```

To keep the BuckleScript transpiler running in watch mode:

```
npm run start
```

If the build becomes "stale", here's how to rebuild it:

```
npm run rebuild 
```

## Update

This should update the package and its dependencies to the latest and rebuild everything

```
npm run update
```


## Developing gcl-atom along side gcl-vscode

Since `gcl-atom` depends on `gcl-vscode`, it would be nice for `gcl-atom` to have direct access to `gcl-vscode` on the disk, so that any changes made to `gcl-vscode` would also reflect on `gcl-atom`.

To achieve this, we first we need to create a "link". 
Assuming `gcl-atom` and `gcl-vscode` are siblings in the same directory.
This should create a link called "guacamole":

```
cd gcl-vscode/
yarn link
```

This should redirect the dependency to the one on you disk.

```
cd ../gcl-atom
yarn link guacamole
```

However, this would create 2 copies of `react` on the `gcl-atom` side, and [break the view](https://reactjs.org/warnings/invalid-hook-call-warning.html). To solve this, we will do the trick again. This time we are creating a link for `react`.

First, go to `gcl-vscode`'s `node_modules/react` (assuming you have executed `npm install` on `gcl-vscode`), and create the link called `react`.

```
cd gcl-vscode/node_modules/react
yarn link
```

Then go back to `gcl-atom` to use the link:

```
cd ../gcl-atom
yarn link react
```

## Files

```
gcl-atom/
├── keymaps/              -- Atom key-command mappings ([doc](https://flight-manual.atom.io/behind-atom/sections/keymaps-in-depth/))
├── lib/                  -- JS files transpiled from src/
├── menus/                -- Atom menu
├── src/                  -- Code
├── styles/               -- CSS (LESS) stylesheets for the view
├── bsconfig.json/        -- BuckleScript config
├── package.json/         -- Node.js package config
└── webpack.config.js     -- Bundles src/ to lib/              
```

inside `src/`

```
src/
├── Editor/               -- For manipulating the editor
├── GCL/                  -- Responses coming from GCL
├── Instance/             -- The "State"
├── Node/                 -- Node.js bindings
├── Task/                 -- For manipulating `Instance`
├── Util/                 
├── View/                 -- View constructed with React
├── Connection.re         -- Connection to GCL
├── Main.re               -- Entry point
└── Types.re              
```
