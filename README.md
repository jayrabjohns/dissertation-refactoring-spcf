# Refactoring SPCF

## Running locally with GHC and Cabal.
Firstly, you'll need GHC and Cabal, which can be installed with [GHCup](https://www.haskell.org/ghcup/install/).

1. 
```shell
cabal update
cabal run
```

2. Parse an SPCF file
```shell
cabal run spcf programs/test.spcf
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

## Useful reading
[Building Interpreters by Composing Monads](https://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.46.9009)

[Monad Transformers Step by Step](http://patryshev.com/books/Transformers.pdf)

[How to build a monadic interpreter in one day](https://wiki.haskell.org/wikiupload/c/c6/ICMI45-paper-en.pdf)

[Monad Transformers and Modular Interpreters](http://web.cecs.pdx.edu/%7Empj/pubs/modinterp.html)



Worth noting that the bounded SPCF contains n natural numbers in its definition directly -> no need for church representation.


STLC impl has pass by name semantics -> application does not evaluate the rhs beforehand?


It seems that the runtime tpye information is only needed for the counting of reduction steps. This makes sense since it requires a sense for how many more reductions will be necessary at any given time during the evaluation.


The evaluation itself doesn't need the type information. This is analogous to running an executeable, type information is lost.

But for the refactoring step type information will most likely be needed. I suppose it's a nice intersection between run time computation being performed at program time.¬