#!/bin/bash

source env.sh

raw="$TX_PATH/create-cat-ref.raw"
signed="$TX_PATH/create-cat-ref.signed"

USER=$1
REF_SCR_WALLET_NAME=$2

DEB_ADDR=$(cat $WALLET_PATH/$USER.addr)

cur_slot=$(cardano-cli query tip --testnet-magic ${TESTNET_MAGIC} | jq '.slot')
currentTime=$(($(date +%s) * 1000))

cabal run write-data Policy CAT $CAT_Policy $(("$currentTime" + 3600000)) $DEB_ADDR
cardano-cli conway transaction policyid --script-file $CAT_Policy > $CAT_CS

cabal run write-data Validator Controller $CONTROLLER $(cat $CAT_CS) $DEB_ADDR
cardano-cli address build --testnet-magic ${TESTNET_MAGIC} --payment-script-file $CONTROLLER --out-file $CONTROLLER_ADDR
cardano-cli conway transaction policyid --script-file $CONTROLLER > $CONTROLLER_VH

UTXO_IN=$(get_address_biggest_lovelace $(cat $WALLET_PATH/$USER.addr))

cardano-cli conway transaction build \
    --testnet-magic ${TESTNET_MAGIC} \
    --tx-in $UTXO_IN \
    --tx-in-collateral $UTXO_IN \
    --tx-out $(cat $WALLET_PATH/$REF_SCR_WALLET_NAME.addr)+17481360 \
    --tx-out-reference-script-file $CAT_Policy \
    --change-address $DEB_ADDR \
    --out-file $raw 

cardano-cli conway transaction sign \
    --testnet-magic ${TESTNET_MAGIC} \
    --tx-body-file $raw \
    --signing-key-file $WALLET_PATH/$USER.skey \
    --out-file $signed 

cardano-cli conway transaction submit \
    --testnet-magic ${TESTNET_MAGIC} \
    --tx-file $signed 

tx_submitted $signed $(cat $WALLET_PATH/$2.addr)
