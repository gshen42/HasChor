# HasChor

HasChor is a library for functional choreographic programming in Haskell.

- HasChor provides a monadic interface for choreographic programming where
choreographies are expressed as computations in a monad.

- Based on Haskell's type system, HasChor supports *higher-order choreographies*
and *location polymorphism*, both features that enable modularity and code
reuse.

- HasChor's implementation is flexible, extensible, and concise (less than 150
lines of code for the core implementation; the swappable backends add another 150 lines)
thanks to the mixed embedding technique it uses. Freer monads
let us reuse existing Haskell constructs as much as possible while
interpreting choreographic primitives freely.

See our [ICFP 2023 paper](https://doi.org/10.1145/3607849) for more details.
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
  - [`Choreography/Choreo.hs`](src/Choreography/Choreo.hs) defines the `Choreo` monad and the `epp` function.
- [`examples`](examples) is the root directory of HasChor examples.
  - [`bank-2pc`](examples/bank-2pc) defines a two-phase commit bank.
  - [`bookseller-0-network`](examples/bookseller-0-network) defines the bookseller protocol as separate network programs.
  - [`bookseller-1-simple`](examples/bookseller-1-simple) defines the bookseller protocol as a simple choreography.
  - [`bookseller-2-higher-order`](examples/bookseller-2-higher-order) defines the bookseller protocol as a higher-order choreography.
  - [`bookseller-3-loc-poly`](examples/bookseller-3-loca-poly) defines a location-polymorphic bookseller protocol.
  - [`diffiehellman`](examples/diffiehellman) defines the diffie-hellman key exchange protocol.
  - [`kvs-1-simple`](examples/kvs-1-simple) defines a simple client-server key-value store.
  - [`kvs-2-primary`](examples/kvs-2-primary) defines a key-value store with a single backup.
  - [`kvs-3-higher-order`](examples/kvs-3-higher-order) defines a higher-ordered key-value store that abstracts over replication strategies.
  - [`kvs-4-loc-poly`](examples/kvs-4-loc-poly) defines a key-value store that utilizes a location-polymorphic function to support 2 backups.
  - [`mergesort`](examples/mergesort) defines a three-way concurrent merge sort.
  - [`playground`](examples/playground) defines a template for wirting your own choreography.
  - [`ring-leader`](examples/ring-leader) defines an experimental ring leader election protocol.

## Building the library

HasChor is a Haskell library and built with the Glasgow Haskell Compiler (GHC).
You can either [build from the source](#building-from-the-source) or [use the Docker image](#using-the-docker-image).

### Building from the source

HasChor is tested with the following Haskell environment:

- [GHC](https://www.haskell.org/ghc/) (9.2.8)
- [Cabal](https://www.haskell.org/cabal/) (3.6.2.0)

Newer versions might work but are not guaranteed.
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

HasChor comes with a set of illustrative example choreographies located in the [`examples`](examples) directory.
To run these examples, do:

```bash
cabal run <executable-name> <location>
```

The `<executable-name>` for each example can be found in the executable section of the [`HasChor.cabal`](HasChor.cabal) file,
and they usually match with their directory name in [`examples`](examples).

The `<location>` specifies what location the choreography should be projected to and run.
In most cases, you would open up multiple shells and run the choreography with a participating location in each of them to simulate a distributed system.
It is crucial that you run the choreography with all participanting locations and roughly at the same time, otherwise the system would deadlock or throw an error.
Most examples come with a `README.md` in their directory that details the commands to run them properly.

As an example, here we show how to run the [`bookseller-1-simple`](examples/bookseller-1-simple) example.
First, check out the [`README.md`](examples/bookseller-1-simple/README.md) for general information about the example.
To run the example, do:

```bash
# in shell 1
cabal run bookseller-1-simple buyer

# in shell 2
cabal run bookseller-1-simple seller

# shell 1 will prompt the user to type in the book they want to buy
> Enter the title of the book to buy
Types and Programming Languages

# shell 1 will return the delivery date it receives from the seller
# then both programs terminate
> The book will be delivered on 2022-12-19
```

## Writing your own choreography

The easist way to write your choreography is to use the [`playground`](examples/playground)
example where we provide a template for writing your own choreography.
Edit the [`Main.hs`](examples/playground/Main.hs) file and run the chreography as follows:

```bash
cabal run playground <location>
```

If you want to depend on HasChor in your own package, add this to your `cabal.project`.

``` cabal-config
source-repository-package
    type: git
    location: https://github.com/gshen42/HasChor.git
    branch: main
```

## Notes for artifact evaluation

### Step 1: Obtaining and building the artifact

See [Building the library](#building-the-library) from above.
The following steps assume the reviewer is in the root directory of the repo.

### Step 2: Running the examples from the paper

In this step we describe how to run each of the examples from our paper. Most of
the examples are interactive programs requiring command-line inputs from the
user, so this is not a "press one key and walk away" step.

#### Step 2.1: Bookseller examples

These examples use a protocol that defines an interaction between two
participants: a seller and a (would-be) buyer. The protocol begins with the
buyer sending the title of a book they want to buy to the seller. The seller
replies with the book’s price, and the buyer checks if the price is within their
budget. If the buyer can afford the book, they inform the seller and get back a
delivery date; otherwise, they tell the seller they will not buy the book.

In these examples, we assume the only books that a buyer can buy are "Types and
Programming Languages" and "Homotopy Type Theory", as `priceOf` and `deliveryOf`
are partial functions:

```haskell
priceOf :: String -> Int
priceOf "Types and Programming Languages" = 80
priceOf "Homotopy Type Theory"            = 120

deliveryDateOf :: String -> Day
deliveryDateOf "Types and Programming Languages" = fromGregorian 2022 12 19
deliveryDateOf "Homotopy Type Theory"            = fromGregorian 2023 01 01
```

It's straightforward to extend these definitions and make them total.

By default, the buyer's `budget` is set to `100`.

##### Bookseller as individual network programs

To run a simple, non-choreographic "bookseller" example from the paper (shown in
Figures 1 and 2), open two shells and then run the following commands from the
project directory:

```bash
# in shell 1
cabal run bookseller-0-network buyer

# in shell 2
cabal run bookseller-0-network seller
```

The buyer can then interact by typing in the name of the book they want to
buy. For example:

```bash
# in shell 1
> Enter the title of the book to buy
Types and Programming Languages

# in shell 1
> The book will be delivered on 2022-12-19
```

#### Bookseller as a simple choreography

Next, to run the simple choreographic bookseller example from the paper (shown
in Figures 3 and 4) with a book different than before:

```bash
# in shell 1
cabal run bookseller-1-simple buyer

# in shell 2
cabal run bookseller-1-simple seller

# in shell 1
> Enter the title of the book to buy
Homotopy Type Theory

# in shell 1
> The book's price is out of the budget
```

#### Higher-order bookseller

To run the higher-order choreographic bookseller example from the paper (shown in Figure 5) with `mkDecision2`:

```bash
# in shell 1
cabal run bookseller-2-higher-order buyer

# in shell 2
cabal run bookseller-2-higher-order buyer2

# in shell 3
cabal run bookseller-2-higher-order seller

# in shell 1
> Enter the title of the book to buy
Homotopy Type Theory

# in shell 2
> How much you're willing to contribute?
100

# in shell 1
The book will be delivered on 2023-01-01
```

Note that previously, the buyer couldn't buy `Homotopy Type Theory` as it was out of the
budget, but with buyer2's contribution, now they can.

#### Location-polymorphic bookseller

Finally, to run the location-polymorphic choreographic bookseller example from the paper (shown in Figure 6):

```bash
# in shell 1
cabal run bookseller-3-loc-poly buyer

# in shell 2
cabal run bookseller-3-loc-poly seller

# in shell 1
> Enter the title of the book to buy
Types and Programming Languages

# in shell 1
> The book will be delivered on 2022-12-19
```

#### Step 2.2: Key-value store examples

These examples define a protocol for a key-value store (KVS). A client sends
requests to a server, and the server handles requests and sends responses back
to the client. The server supports two kinds of requests: `PUT`, to set a given
key-value pair, and `GET`, to look up the value associated with a specified key.

Both the client and server run in an infinite loop waiting for commands.

##### Simple KVS

To run the simple choreographic key-value store example from the paper (shown in
Figures 7 and 8):

```bash
# start server
cabal run kvs1 server
# on a different terminal for client
cabal run kvs1 client
```

The client will then be prompted for a command, and the user can input `PUT` and
`GET` commands.  An example interaction on the client side might look like the
following:

```bash
# on the client terminal
Command?
GET hello
> Nothing
Command?
PUT hello world
> Just "world"
Command?
GET hello
> Just "world"
```

##### Primary-backup KVS

To run the primary-backup key-value store example from the paper (shown in Figure 9):

```bash
# start primary
cabal run kvs2 primary

# on a different terminal, start backup
cabal run kvs2 backup

# another terminal for client
cabal run kvs2 client
GET hello
> Nothing
PUT hello world
> Just "world"
GET hello
> Just "world"
```

##### Higher-order KVS


To run the higher-order key-value store example from the paper (shown in Figure 10):

```bash
# start primary
cabal run kvs3 primary

# on a different terminal, start backup
cabal run kvs3 backup

# another terminal for client
cabal run kvs3 client
GET hello
> Nothing
PUT hello world
> Just "world"
GET hello
> Just "world"
```

##### Location-polymorphic and higher-order KVS

To run the (location-polymorphic and higher-order) double-backup key-value store example from the paper (shown in Figures 11 and 12):

```bash
# start primary
cabal run kvs4 primary

# on a different terminal, start backup1
cabal run kvs4 backup1

# another terminal for backup2
cabal run kvs4 backup2

# yet another terminal for client
cabal run kvs4 client
GET hello
> Nothing
PUT hello world
> Just "world"
GET hello
> Just "world"
```

### Step 2.3: Brief tour of the implementation

See [Directory structure](#directory-structure) from above.

