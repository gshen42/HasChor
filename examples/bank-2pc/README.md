# Example: bank-2pc

## Overview

This example implements the two-phase commit protocol.

This system models a bank. Alice and Bob each have their accounts on the bank, and the balance of each account is stored at separate locations `alice` and `bob`. This bank has a policy that does not allow balances to be negative.

As a bank administrator, you can change the balance of accounts. An `Action` changes a single account's balance by specifying the account (`String`, "alice" or "bob") and the amount to add to the account (Int, can be negative to indicate withdrawal). A `Transaction` is a list of `Action`s.

When executing a `Transaction`, the system will perform Alice's actions at `alice` and Bob's actions at `bob`. However, the transaction aborts if one of the actions violates the bank policy and makes the balance negative. We use the two-phase commit protocol to ensure consistency between `alice` and `bob`.

In the voting phase, `alice` and `bob` check if the given transaction violates the policy. If one or more locations determine that the transaction violates the policy, the entire transaction aborts. If the transaction is valid, each location will commit the transaction.

## Execution

For simplicity, this example uses `runChoreo` and executes the choreography directly. On the terminal, you can write transactions as follows. Both accounts' balances are initialized to 0.

```text
> cabal run bank-2pc
Command? (alice|bob {amount};)+
alice 10; bob 10                  # deposit 10 each to Alice and Bob's accounts
Committed
Alice's balance: 10
Bob's balance: 10
Command? (alice|bob {amount};)+   # deposit 20 to Alice's and withdraw 10 from Bob's
alice 20; bob -5
Committed
Alice's balance: 30
Bob's balance: 5
Command? (alice|bob {amount};)+   # move 10 from Bob to Alice (Invalid, Bob's account will be negative)
alice 10; bob -10
Not committed
Alice's balance: 30
Bob's balance: 5
```
