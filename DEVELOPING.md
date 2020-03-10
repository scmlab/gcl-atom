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
cd <cloned directory> && npm install
npm run build
```

To open some file in the development mode:

```
atom -d file.gcl
```

To keep the BuckleScript transpiler running in watch mode:

```
npm run start
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
