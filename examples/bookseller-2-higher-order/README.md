# Higher-order bookseller

This example implements the higher-order bookseller protocol where
how the buyer decides whether to buy the book is abstracted as an
argument, making it a higher-order choreography.
For a general description of the protocol, see [`bookseller-1-simple`](../bookseller-1-simple).

We provide two decision-making functions:

- `mkDecision1` only checks if buyer's budget is greater than the
  book's price, which has the same behavior as [`bookseller-1-simple`](../bookseller-1-simple)
- `mkDecision2` asks buyer2 how much they're willing to contribute and
  checks if the buyer's budget is greater than the book's price minus
  buyer2's contribution.

By default, this example uses `mkDecision2`. To use `mkDecision1`,
change the line `choreo = bookseller mkDecisiont2` to `choreo = bookseller mkDecision1`.

## Running the protocol

```bash
# in shell 1
cabal run bookseller-higher-ordered buyer

# in shell 2
cabal run bookseller-higher-orderer buyer2

# in shell 3
cabal run bookseller-higher-ordered seller

# in shell 1
> Enter the title of the book to buy
Homotopy Type Theory

# in shell 2
> How much you're willing to contribute?
100

# in shell 1
The book will be delivered on 2023-01-01
```

Note previously in [`bookseller-1-simple`](../bookseller-1-simple),
the buyer can't buy `Homotopy Type Theory` as it's out of the budget,
but with buyer2's contribution, now it can.
