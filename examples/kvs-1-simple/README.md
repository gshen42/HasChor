# Example: Simple client-server key-value store

This is the first version of the 4-stage key-value store tutorial and implements a simple client-server key-value store. The client can `PUT` a key-value pair to the server and `GET` a value for a given key.

## Execution

```bash
# start server
cabal run kvs1 server
# on a different terminal for client
cabal run kvs1 client
GET hello
> Nothing
PUT hello world
> Just "world"
GET hello
> Just "world"
```
