# Example: merge sort

## Overview

This example implements the three-way concurrent implementation of merge sort shown in [Object-Oriented Choreographic Programming](https://arxiv.org/abs/2005.09520).

## Execution

This choreography will sort the list `[1, 6, 5, 3, 4, 2, 7, 8]`. It requires three locations: `primary`, `worker1`, and `worker2`.

```bash
# start two workers on separate terminals
cabal run mergesort worker1
cabal run mergesort worker2
# start primary on another terminal
cabal run mergesort primary
[1,2,3,4,5,6,7,8]
```
