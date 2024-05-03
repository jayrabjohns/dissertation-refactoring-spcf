# SPCF Interpreter

This project is submitted as part of my dissertation while studying at the University of Bath.

## Abstract 
We explore the concept of function denesting in SPCF, a derivative of typed $\lambda$-calculus with non-local control. Function nesting involves sharing variables from one function’s scope with another function's arguments. This transformation is based on the work of Laird who provides denotational semantics for a retraction on terms of a higher type into a lower one, and can be seen as rewriting programs to adhere to affine typing rules. We illustrate this transformation working in the real world and evaluate its benefits. 

As part of the project we also build and release an interpreter for SPCF, a language that currently lacks one which is widely available. This both has the focus of illustrating the denesting action working, as well as contributing to the language’s ecosystem, hopefully making it easier to experiment with the language.

## Dissertation
The full dissertation along with implementation details of the interpreter is found here: 

## Building locally with GHC and Cabal.
Firstly, you'll need GHC and Cabal, which can be installed with [GHCup](https://www.haskell.org/ghcup/install/).

Then:
```shell
cabal update
cabal build
```

## Parsing program files 

Using `cabal run` and the executeable `spcf` we can pass in a file name to interpret.
```shell
cabal run -- spcf programs/test.spcf
```

## SPCF files
SPCF files are a series of binding statements and evaluations.

The following is an example SCPF file which
1. Binds the variable `x` to the value 3 and the variable `y` to the value 5.
2. Then it binds an abstraction (a function) to the variable `addLeftTerm`.
3. Evaluates the application of `x` and `y` to `addLeftTerm`. 
```scala
x = 3;
y = 5;
addLeftTerm = \f:Nat->Nat->Nat => \x:Nat => \y:Nat => if x then y else (succ (f (pred x) y));
add = \x:Nat => \y:Nat => (fix addLeftTerm) x y
eval (
    add x y
);
```

The `fix` term is taking the fixed point of `addLeftTerm`, effectively making it recursive. It will repeatedly apply the term to itself until the function hits a fixed point.

## Testing 
Unit tests are run in a github action on completion of a pull request.

To run locally:
```shell
cabal test
```

## Useful reading
[Building Interpreters by Composing Monads](https://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.46.9009)

[Monad Transformers Step by Step](http://patryshev.com/books/Transformers.pdf)

[How to build a monadic interpreter in one day](https://wiki.haskell.org/wikiupload/c/c6/ICMI45-paper-en.pdf)

[Monad Transformers and Modular Interpreters](http://web.cecs.pdx.edu/%7Empj/pubs/modinterp.html)


Worth noting that the bounded SPCF contains n natural numbers in its definition directly -> no need for church representation.

It seems that the runtime type information maynot be needed and could be removed. 

The evaluation itself doesn't need the type information. This is analogous to running an executeable, type information is lost.

But for the refactoring step type information will most likely be needed. I suppose it's a nice intersection between run time computation being performed at program time.