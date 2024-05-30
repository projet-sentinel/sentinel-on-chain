#!/bin/bash

source env.sh

killall cardano-node >/dev/null
killall cardano-node >/dev/null
killall cardano-node >/dev/null 

if [[ -d "$NETWORK_DIR_PATH" ]]; then
        rm -r "$NETWORK_DIR_PATH"
    fi

    mkdir $NETWORK_DIR_PATH
    mkdir $TX_PATH
    mkdir $WALLET_PATH
    mkdir $Validator_Path
    mkdir $Policy_Path
    mkdir $StakeVal_Path
    mkdir $REDEEMER_PATH
    mkdir $DATUM_PATH

cd $REPO_HOME/cardano-private-testnet-setup
./scripts/automate.sh || exit 1
