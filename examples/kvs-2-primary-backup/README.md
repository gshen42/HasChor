# Example: Primary-backup key-value store

This is the second version of the 4-stage key-value store tutorial. This builds on the first version and adds a backup location to improve durability.

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
