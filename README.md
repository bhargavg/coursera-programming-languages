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
$ cd path/to/checkout/directory
$ source ./bin/dev_setup.sh  # setup PATH variable
$
$ # Run tests for week 2
$ run_tests 2
$
$ # Launch REPL
$ repl Functions  # Optionally provide source filename to load
- (* REPL changes the prompt to a `-` character *)
-
```
