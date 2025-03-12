#!/bin/bash

source env.sh

USER=$1
INDEX=$2
REQ_WALLET_NAME=$3

raw="$TX_PATH/tdr-ref-addr-$INDEX.raw"
signed="$TX_PATH/tdr-ref-addr-$INDEX.signed"

UTXO_IN=$(get_address_biggest_lovelace $(cat $WALLET_PATH/$USER.addr))

nowSlotNumber=$(get_current_slot_number)

cabal run write-data Datum TDRDatum $DATUM_PATH/tdr-dat$INDEX.json $(cat $WALLET_PATH/$USER.addr) 1 $(cat $WALLET_PATH/$REQ_WALLET_NAME.addr) $nowSlotNumber

cardano-cli conway transaction build \
    --testnet-magic ${TESTNET_MAGIC} \
    --tx-in $UTXO_IN \
    --tx-in-collateral $UTXO_IN \
    --tx-out $(cat $Validator_Path/threatDatabaseRequest$INDEX.addr)+5000000 \
    --tx-out-inline-datum-file $DATUM_PATH/tdr-dat$INDEX.json \
    --change-address $(cat $WALLET_PATH/$USER.addr) \
    --out-file $raw 

cardano-cli conway transaction sign \
    --testnet-magic ${TESTNET_MAGIC} \
    --tx-body-file $raw \
    --out-file $signed \
    --signing-key-file $WALLET_PATH/$USER.skey

cardano-cli conway transaction submit \
    --testnet-magic ${TESTNET_MAGIC} \
    --tx-file $signed

tx_submitted $signed $(cat $Validator_Path/threatDatabaseRequest$INDEX.addr)