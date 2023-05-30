# HasChor

HasChor is a library for functional choreographic programming in Haskell.

- HasChor provides a monadic interface for choreographic programming where
choreographies are expressed as computations in a monad.

- Based on Haskell's type system, HasChor supports *higher-order choreographies*
and *location polymorphism*, both features that enable modularity and code
reuse.

- HasChor's implementation is flexible, extensible, and concise (less than 300
lines of code) thanks to the mixed embedding technique it uses—freer monads,
which allows it to reuse existing Haskell constructs as much as possible while
interpreting choreographic primitives freely.

See our [ICFP 2023 paper (preprint)](https://arxiv.org/abs/2303.00924) for more deatils.
For API specifications, check out the [documentation](https://gshen42.github.io/HasChor/).

HasChor's design is heavily influenced by [Pirouette](https://dl.acm.org/doi/10.1145/3498684).
For a general introduction to choreographic programming, we recommend
[Fabrizio Montesi](https://www.fabriziomontesi.com)'s excellent book:
[Introduction to Choreographies](https://www.fabriziomontesi.com/introduction-to-choreographies/).

## Getting started

HasChor is a Haskell library and built with the Glasgow Haskell Compiler (GHC).
The first thing to do is to build HasChor; you can either [build from the source](#building-from-the-source)
or [use the Docker image](#using-the-docker-image).
After successfully building HasChor, you can run example choreographies or write your own choreography.

### Building from the source

HasChor is tested with the following Haskell environment:

- [GHC](https://www.haskell.org/ghc/) (9.2.6)

- [Cabal](https://www.haskell.org/cabal/) (3.6.2.0)

We recommend using [GHCup](https://www.haskell.org/ghcup/) to set up the environment.

To build HasChor, run the following commands:

```bash
git clone https://github.com/gshen42/HasChor.git
cd HasChor
cabal build -j20
```

### Using the Docker image

First, clone the repo and build the Docker image:

```bash
git clone https://github.com/gshen42/HasChor.git
cd HasChor
docker build -t haschor .
```

Next, create a container and run Bash in it:

```bash
docker run -it -name foo haschor bash
```

You should be presented with HasChor's root directory with everything built.

To create another Bash shell in the container for running and testing
choreographies, use the following command:

```bash
docker exec -it foo bash
```

To remove the created container, use the following command:

```bash
docker rm foo
```

### Running example choreographies

HasChor comes with a set of illustrative examples located in the `examples` directory.
Use `cabal run <executable-name>` to build and run the examples.
The executable name for each example can be found in the [`HasChor.cabal`](HasChor.cabal) file.

### Write your own choreography

TODO

## Special notes for artificial evluation

### Directory structure
