# The Hash Programming language

## Instructions to run the program
This application uses cabal to compile and bind dependencies to the project.


## Install

- You must install all the project dependencies using `cabal update` and then `cabal install`. Once all of the dependencies are installed, the project can run on the system.

- **Note:** If building on lab machines, you might need to restrict the number of jobs that cabal uses to install, compile and link the project. You can do this by running `cabal install -j1` where the `j` argument is the number of job threads cabal should use.

## Run

- Using the command `cabal run hash`. This will compile, build and run the program in the current terminal/shell.


### Debug mode

- Debugging a compiler can be hard, especially when Haskell doesn't like providing a stack trace or any kind of information about call sites by default. So, to run in debug mode you can:
- - Enable `profiling` within `cabal.project.local`
- - Build the compiler with `cabal build --flags debug`
- - Run hash and test the condition.

With debug mode, you will get informative call stacks:

```
$ hash
>>> :p a = 2;
Sorry :^(
Internal Panic: User invoked panic!
CallStack (from -prof):
  Panic.internalPanic' (src\Panic.hs:(10,1)-(17,5))
  Panic.internalPanicPure (src\Panic.hs:21:1-72)
  Typecheck.Traverse.getNodeType (src\Typecheck\Traverse.hs:(241,3)-(267,46))
  Typecheck.Traverse.CAF (<entire-module>)

This is an interpreter bug, please file a bug report at:
    
            https://github.com/feds01/hash/issues
```

Without debug mode, you won't get an informative call stack from haskell:

```
$ hash
Hash Interpreter v0.0.1
>>> :p a = 2;
Sorry :^(
Internal Panic: User invoked panic!

This is an interpreter bug, please file a bug report at:
            
            https://github.com/feds01/hash/issues
```


## Documentation 

- Would you like to look at the source documentation in a presentable way, by running the command cabal `haddock --haddock-executable hash`, Cabal will generate a HTML document that has documentation for all functions within the source.

## Testing and Development

### Starting with the required modules GHCi

- Since this project depends on some external libraries such as `yaml` and `aeson`, using the normal `ghci` command and attempting to import the project modules will not work. This is because `ghci` is un-aware that these modules exist.

- Instead, run a repl using cabal by invoking `cabal new-repl`. This will load the modules that may be needed by some file in the project. Here is an example run:

```
$ cabal new-repl 
Resolving dependencies...
Build profile: -w ghc-8.8.4 -O1
In order, the following will be built (use -v for more details):
 - project-0.1.0.0 (lib) (configuration changed)
Configuring library for project-0.1.0.0..
Preprocessing library for project-0.1.0.0..
Warning: No exposed modules
GHCi, version 8.8.4: https://www.haskell.org/ghc/  :? for help
Prelude>
```

- By using the cabal repl, we can perform interactive development with any required build deps


### Running the application test suite
- You can run the application tests by invoking the command `cabal new-test --enable-tests`
- Here is an example run

```
$ cabal new-test --enable-tests
Resolving dependencies...
Build profile: -w ghc-8.8.4 -O1
In order, the following will be built (use -v for more details):
 - project-0.1.0.0 (test:test) (configuration changed)
Warning: project.cabal:48:3: invalid subsection "expo"
Configuring test suite 'test' for project-0.1.0.0..
Preprocessing test suite 'test' for project-0.1.0.0..
Building test suite 'test' for project-0.1.0.0..
Running 1 test suites...
Test suite test: RUNNING...
Test suite test: PASS
Test suite logged to:
/Users/feds01/Documents/ComputerScience/CS2006/haskell/hash/dist-newstyle/build/x86_64-osx/ghc-8.8.4/project-0.1.0.0/t/test/test/project-0.1.0.0-test.log
1 of 1 test suites (1 of 1 test cases) passed.
```
    
### Submitting a PR
    
Before submitting a PR, make sure that:

- Your code is properly formatted with `ormolu`.
See <https://hackage.haskell.org/package/ormolu> for information about how to install it.
There are plugins available for popular text editors.
Check with `ormolu -m check {tests,src}/*.hs`.
You can use the in-place mode (`-m inplace`) to apply the suggested changes.

- Your code passes the `hlint` test.
See <https://github.com/ndmitchell/hlint/blob/master/README.md#installing-and-running-hlint> for information about how to install it.
Once again, there are text editor plugins available.
Run `hlint src` to lint source and `hlint tests` to lint tests.

- Your code builds successfully, and all the tests run successfully.
