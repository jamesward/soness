# checkout

The checkout of our web shop: pricing, discount codes (SAVE10, WELCOME5), shipping (free above a threshold, or with
the FREESHIP code) and receipts.

Build and test with the sbt launcher in this directory: `./sbt test`, or `./sbt testQuick` to run only the tests
affected by changes since the last run. `CheckoutSimulationSpec` is slow (it simulates a day of orders end to end).
