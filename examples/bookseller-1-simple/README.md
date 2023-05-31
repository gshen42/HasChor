# Simple bookseller

The bookseller protocol defines an interaction between two participants: a
seller and a (would-be) buyer.  The protocol begins with the buyer sending the
title of a book they want to buy to the seller.  The seller replies with the
book’s price, and the buyer checks if the price is within their budget.  If the
buyer can afford the book, they inform the seller and get back a delivery date;
otherwise, they tell the seller they will not buy the book.

In this example, we assume the only books that a buyer can buy are `Types and
Programming Languages` and `Homotopy Type Theory` as `priceOf` and `deliveryOf`
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

## Running the example

```bash
# in shell 1
cabal run bookseller-1-simple buyer

# in shell 2
cabal run bookseller-1-simple seller

# shell 1 will prompt the user to type in the book they want to buy
> Enter the title of the book to buy
Types and Programming Languages

# shell 1 will return the delivery date it receives from the seller or out of budget
# then both programs terminate
> The book will be delivered on 2022-12-19


# if we run the choreography again with a different book to buy

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
