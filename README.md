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

## Directory structure

- [`src`](src) is the root directory of HasChor library.
  - [`Control/Monad/Freer.hs`](src/Control/Monad/Freer.hs) defines the `Freer` monad.
  - [`Choreography.hs`](src/Choreography.hs) defines the top-level interface to the library.
  - [`Choreography/Location.hs`](src/Choreography/Location.hs) defines type-level and term-level locations: `LocTy` and `LocTm`.
  - [`Choreography/Network.hs`](src/Choreography/Network.hs) defines the `Network` monad and the `Backend` typeclass.
  - [`Choreography/Network/Local.hs`](src/Choreography/Network/Local.hs) defines the local multi-threaded backend.
  - [`Choreography/Network/Http.hs`](src/Choreography/Network/Http.hs) defines the HTTP backend.
  - [`Choreography/Choreo.hs`](src/Choreography/Choreo.hs) defines the `Choreo` monad and the `epp` funciton.
- [`examples`](examples) is the root directory of HasChor examples.
  - [`bookseller-0-network`](examples/bookseller-0-network) defines the bookseller protocol as seperate network programs.
  - [`bookseller-1-simple`](examples/bookseller-1-simple) defines the bookseller protocol as a simple choreography.
  - [`bookseller-2-higher-order`](examples/bookseller-2-higher-order) defines the bookseller protocol as a higher-ordered choreography.
  - [`kvs-1-simple`](examples/kvs-1-simple) defines a simple client-server key-value store.
  - [`kvs-2-primary`](examples/kvs-2-primary) defines a key-value store with a single backup.
  - [`kvs-3-hoc`](examples/kvs-3-hoc) defines a higher-ordered key-value store that abstracts over replication strategies.
  - [`kvs-4-location-polymorphism`](examples/kvs-4-location-polymorphism) defines a key-value store that utilizes a location-polymorphic function to support 2 backups.

## Building the library

HasChor is a Haskell library and built with the Glasgow Haskell Compiler (GHC).
You can either [build from the source](#building-from-the-source) or [use the Docker image](#using-the-docker-image).

### Building from the source

HasChor is tested with the following Haskell environment:

- [GHC](https://www.haskell.org/ghc/) (9.2.6)
- [Cabal](https://www.haskell.org/cabal/) (3.6.2.0)

Newer versions might work but areographical not guaranteed.
We recommend using [GHCup](https://www.haskell.org/ghcup/) to set up the environment.

To build HasChor, do:

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

To create another Bash shell in the container for running and testing choreographies, do:

```bash
docker exec -it foo bash
```

To remove the created container, do:

```bash
docker rm foo
```

## Running example choreographies

HasChor comes with a set of illustrative examples located in the `examples` directory.
Use `cabal run <executable-name>` to build and run the examples.
The executable name for each example can be found in the [`HasChor.cabal`](HasChor.cabal) file.

## Write your own choreography

TODO

## Note for artifact evaluation
