# Example: bookseller

## Overview

This example implements the `bookseller` protocol.

## Execution

`buyer` requires the title of the book to purchase. When prompted, type `Types and Programming Languages` or `Homotopy Type Theory`.

```bash
# run seller
cabal run bookseller seller
# on another terminal, run buyer
cabal run bookseller buyer
Enter the title of the book to purchase
Types and Programming Languages
2022-12-19
```
