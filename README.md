## Prerequisits 

- [cardano-node](https://github.com/IntersectMBO/cardano-node) version 8.9.0
- [nix](https://nixos.org/download/#download-nix)
- [hydra](https://github.com/input-output-hk/hydra)
    - `git checkout 0.16.0`
    - `nix develop`
- `REPO_HOME` in `/scripts/env.sh` should point to this repository
- `cd scripts`
- `chmod +x 00-setup.sh`
- `./00-setup.sh`


## run examples
- `cabal build all`
- `cd run-examples`
- ./run-full-trace.sh`

## transaction scripts
can be found in `scripts`

### private testnet
- 00-setup.sh
    - make all scripts executable
    - clone gitlab repository to run private testnet

- 01-start-private-testnet.sh
    - delete and create all repositories
    - start private testnet

### administration
- 02-create-cat.sh
    - create controller authentication policy reference script 
    - will be used to identify utxos at controller validator
    - #### arguments
        - `$1` = name of any wallet in `$WALLET_PATH` (with enough funds)
        - `$2` = reference wallet name

- 03-create-bmat.sh
    - mint board member authentication token
    - lock at controller validator with list of pub key hashes in datum
    - #### arguments
        - `$1` = name of board member wallet in `$WALLET_PATH` (with enough funds)

- 04-create-controller.sh
    - create controller validator reference script
    - mint token with `cat` (controller authentication token) policy id and an empty token name
    - all board members need to sign this transaction
    - #### arguments
        - `$1` = name of any wallet in `$WALLET_PATH` (with enough funds)
        - `$2` = name of reference wallet where cat reference is present

### address list as threat data
Threat data provides a list of addresses.

- 05addr-create-oracle-ref.sh
    - mint controller authentication token (`cat`) with threat database authentication (`tdat`) policy hash as token name
    - lock at controller validator with threat database authentication policy as reference script and `TDATRefScriptDatum` as datum
    - mint controller authentication token (`cat`) with threat database validator hash as token name
    - lock at controller validator with threat database validator as reference script and `ThreatDatabaseRefScriptDatum` as datum
    - at least 51% of board members have to sign
    - #### arguments
        - `$1` = name of any wallet in `$WALLET_PATH` (with enough funds)
        - `$2` = name of board member wallet in `$WALLET_PATH` where cat reference script is present
        - `$3` = threat database index
        - `$4` = name of debugger wallet in `$WALLET_PATH` 
        - `$5` = name of oracle admin wallet in `$WALLET_PATH` which will provide data to the oracle
        - `$6` = name of treasury wallet in `$WALLET_PATH`

- 06addr-mint-tdat.sh
    - mint threat database authentication token (`tdat`)
    - lock at threat database validator with `ThreatDatabaseAddrListDatum` as datum
    - oracle admin has to sign
    - #### arguments
        - `$1` = name of oracle admin wallet in `$WALLET_PATH`
        - `$2` = threat database index
        - `$3` = name of malicious wallet in `$WALLET_PATH`
        - `$4` = threat score

- 06b-send-request.sh
    - request threat score for an address
    - lock ada at `ThreatDatabaseRequest` (TDR) validator with datum `TDRDatum`
    - #### arguments
        - `$1` = name of requester wallet name in `$WALLET_PATH`
        - `$2` = threat database index where address should be added
        - `$3` = requested wallet name in `$WALLET_PATH`

- 07a-update-threat-score.sh
    - update threat database address list
    - oracle admin has to sign
    - #### arguments
        - `$1` = name of oracle admin wallet in `$WALLET_PATH`
        - `$2` = threat database index
        - `$3` = new threat score

- 07b-remove-threat-score.sh
    - burn threat database authentication token (`tdat`)
    - oracle admin has to sign
    - sent locked ada to treasury defined in `ThreatDatabaseRefScriptDatum`
    - #### arguments
        - `$1` = name of oracle admin wallet in `$WALLET_PATH`
        - `$2` = threat database index
        - `$3` = name of treasury wallet in `$WALLET_PATH`

- 07c-remove-request.sh
    - remove request from `ThreatDatabaseRequest` validator
    - requester has to sign
    - ada back to requester
    - #### arguments
        - `$1` = name of requester wallet name in `$WALLET_PATH`
        - `$2` = threat database index

- 07d-update-threat-score-req.sh
    - unlock ada from `ThreatDatabaseRequest` validator
    - update threat database address list with address in datum `TDRDatum`
    - oracle admin has to sign
    - #### arguments
        - `$1` = name of oracle admin wallet in `$WALLET_PATH`
        - `$2` = threat database index
        - `$3` = new threat score
        - `$4` = name of treasury wallet in `$WALLET_PATH`
        - `$5` = requested wallet name in `$WALLET_PATH`

- 08-lock-escrow.sh
    - lock ada at escrow contract
    - datum which holds sender and receiver address
    - #### arguments
        - `$1` = name of sender wallet in `$WALLET_PATH`
        - `$2` = name of receiver wallet in `$WALLET_PATH`
        - `$3` = amount to lock
        - `$4` = threat database index to use for validation

- 09-unlock-escrow.sh
    - receiver unlock ada from escrow
    - receiver has to sign
    - `tdat` utxo as read only input
    - #### arguments
        - `$1` = name of receiver wallet in `$WALLET_PATH`
        - `$2` = threat database index of token used

### script hash list as threat data
Threat data provides a list of script hashes.

- 05sh-create-oracle-ref.sh
    - mint controller authentication token (`cat`) with threat database authentication (`tdat`) policy hash as token name
    - lock at controller validator with threat database authentication policy as reference script and `TDATRefScriptDatum` as datum
    - mint controller authentication token (`cat`) with threat database validator hash as token name
    - lock at controller validator with threat database validator as reference script and `ThreatDatabaseRefScriptDatum` as datum
    - at least 51% of board members have to sign
    - #### arguments
        - `$1` = name of any wallet in `$WALLET_PATH` (with enough funds)
        - `$2` = name of board member wallet in `$WALLET_PATH` where cat reference script is present
        - `$3` = threat database index
        - `$4` = name of debugger wallet in `$WALLET_PATH` 
        - `$5` = name of oracle admin wallet in `$WALLET_PATH` which will provide data to the oracle
        - `$6` = name of treasury wallet in `$WALLET_PATH`

- 06sh-mint-tdat.sh
    - mint threat database authentication token (`tdat`)
    - lock at threat database validator with `ThreatDatabaseAddrListDatum` as datum
    - oracle admin has to sign
    - #### arguments
        - `$1` = name of oracle admin wallet in `$WALLET_PATH`
        - `$2` = threat database index
        - `$3` = name of malicious wallet in `$WALLET_PATH`
        - `$4` = threat score

## utility scripts

- balance-validator-detailed.sh 
    - validator utxos with datum in json format
    - #### arguments
        - `$1` = name of validator in `$Validator_Path`

- balance-validator.sh 
    - simple validaotr utxos 
    - #### arguments
        - `$1` = name of validator in `$Validator_Path`

- balance-wallet.sh 
    - simple wallet utxos 
    - #### arguments
        - `$1` = name of wallet in `$WALLET_PATH`

- create-user-stake.sh 
    - creates wallet in `$WALLET_PATH` with staking part
    - #### arguments
        - `$1` = name of new wallet
        
- write-env.sh
    - creates `config.json` file 
    - #### arguments 
        - if `$1` then preprod else private testnet config