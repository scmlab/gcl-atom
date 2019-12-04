# gcl-atom

## Commands

| Keymap                             | Command                                 |
|-----------------------------------:|:----------------------------------------|
| <kbd>C-c</kbd> <kbd>C-x</kbd>      | activate/deactivate the package         |
| <kbd>Cmd-s</kbd> or <kbd>C-s</kbd> | save and reload the file                |
| <kbd>C-c</kbd> <kbd>C-c</kbd>      | refine a hole                           |

## Development setup

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
