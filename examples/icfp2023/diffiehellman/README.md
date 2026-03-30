# Example: diffie-hellman key exchange

## Overview

This example implements the [diffie-hellman key exchange protocol](https://en.wikipedia.org/wiki/Diffie%E2%80%93Hellman_key_exchange).

In this example, two locations, `alice` and `bob`, exchange the secret key without sending the key over the network.

## Execution

To run this example, you will need to run two locations (alice and bob) at the same time. Alice initiates the exchange, and Bob waits for Alice. When both locations are ready, press "enter" on Alice's terminal to start the protocol.

```
> cabal run diffiehellman bob
waiting for alice to initiate key exchange

# on a different terminal
> cabal run diffiehellman alice
enter to start key exchange...
[Enter]

# Alice's terminal
alice's shared key: 1544

# Bob's terminal
bob's shared key: 1544
```

This sample uses [`System.Random`](https://hackage.haskell.org/package/random-1.2.1.1/docs/System-Random.html) and will generate different keys at each invocation.
