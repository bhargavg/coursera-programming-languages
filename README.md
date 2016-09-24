# Notes from: Programming Languages - Dan Grossman (Coursera)

## Install sml 
 On Ubuntu:

```bash
$ sudo apt-get update && sudo apt-get install smlnj
```

On Mac:

```bash
$ brew install smlnj
```

> Note: While not necessary, install `rlwrap` to be able to use arrow keys in sml repl

## Running the `.sml` files
```bash
$ rlwrap sml   # launches REPL inside rlwrap
- (* REPL changes the prompt to a `-` character *)
- (* Load the source file using `use` command *)
- use "filename.sml";
```
