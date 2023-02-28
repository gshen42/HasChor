# Example: Key-value store with location polymorphism

This is the final version of the 4-stage key-value store tutorial where we define the location-polymorphic choreography `doBackup`. We use it to define `doubleBackupReplicationStrategy`, which replicates data to two backup locations (`backup1` and `backup2`).

## Execution

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
