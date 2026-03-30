# Example: Key-value store with higher-order choreography

This is the third version of the 4-stage key-value store tutorial. We refactor the second version using HasChor's higher-order choreography. We define `ReplicationStrategy`, which specifies the details of replication and modify the `kvs` to take a `ReplicationStrategy`.

## Execution

By default, `primaryBackupReplicationStrategy` will be used. Change `mainChoreo` to `nullReplicationChoreo` to use `nullReplicationStrategy`.

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
