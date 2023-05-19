# HasChor

HasChor is a library for functional choreographic programming in Haskell.

- HasChor provides a monadic interface for choreographic programming where choreographies are expressed as computations in a monad.
- Based on Haskell's type system, HasChor supports *higher-order choreographies* and *location polymorphism*, both features that enable modularity and code reuse.
- HasChor's implementation is flexible, extensible, and concise (less than 300 lines of code) thanks to the mixed embedding technique it uses—freer monads, which allows it to reuse existing Haskell constructs as much as possible while interpreting choreographic primitives freely.

See our [ICFP 2023 paper (preprint)](https://arxiv.org/abs/2303.00924) for more deatils.
For API specifications, check out the [documentation](https://gshen42.github.io/HasChor/).

## Usage

### Prerequisites

HasChor is tested with the following Haskell environment:

- [GHC](https://www.haskell.org/ghc/) (9.2.6)
- [Cabal](https://www.haskell.org/cabal/) (3.6.2.0)

We recommend using [GHCup](https://www.haskell.org/ghcup/) to set up the environment.

### Getting Started

*We're working on putting HasChor on Hackage.*
Right now, the easiest way to try out HasChor is to write your program inside HasChor's repo and list it as an executable in the [`HasChor.cabal`](HasChor.cabal) file.

### More Examples

HasChor comes with a set of illustrative examples located in the `examples` directory.
Use `cabal run <executable-name>` to build and run the examples.
The executable name for each example can be found in the [`HasChor.cabal`](HasChor.cabal) file.
