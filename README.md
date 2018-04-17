# parking-dao

[![Build Status](https://travis-ci.org/f-o-a-m/parking-dao.svg?branch=master)](https://travis-ci.org/f-o-a-m/parking-dao)

<img src=https://github.com/f-o-a-m/purescript-web3/blob/master/purescript-web3-logo.png width="75">

_An Example Solidity Application Using the [Chanterelle](https://github.com/f-o-a-m/chanterelle) Build Tool_

## Parking DAO

The Parking DAO is a set of smart contracts that encapsulate a `User`, representing an account which is granted permission to park in certains geographical zones, and a `ParkingAnchor`, representing an account which has the ability to accept payment for parking in certain zones. These accounts are deployed by a central authority called the `ParkingAuthority`, which is a governing contract in charge of account management. The `ParkingAuthority` also contains the logic for altering account permissions.

For more information about how to use the contracts, see [this README](https://github.com/f-o-a-m/parking-dao/blob/master/sequence-diagrams/README.md), or look at the contracts in the `/contracts` directory. You can find the tests in `/test` that verify their behaviour.

## Build / Test / Deploy
This is a chanterelle project, so you should see that page for specific information about how to alter the chanterelle.json if you wish to make changes to this project. Otherwise the workflow is dictated by the `Makefile`:

```bash
> make install
> make compile-contracts
> make deploy
> make test
```
