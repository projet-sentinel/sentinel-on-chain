#!/bin/bash

source env.sh

USER=$1
INDEX=$2

raw="$TX_PATH/tdr-remove-req-$INDEX.raw"
signed="$TX_PATH/tdr-remove-req-$INDEX.signed"

USER_PKH=$(cardano-cli address key-hash --payment-verification-key-file $WALLET_PATH/$USER.vkey)

UTXO_IN=$(get_address_biggest_lovelace $(cat $WALLET_PATH/$USER.addr))
UTXO_IN_REQ=$(get_address_biggest_lovelace $(cat $Validator_Path/threatDatabaseRequest$INDEX.addr))

CC_REF_UTXO=$(get_UTxO_by_token $(cat $CONTROLLER_ADDR) "$(cat $CAT_CS).$(cat $Validator_Path/threatDatabaseRequest$INDEX.vh)")

cabal run write-data Redeemer TDR RemoveRequest $REDEEMER_PATH/remove-request.json

nowSlotNumber=$(get_current_slot_number)
#submissionTime=$(nowSlotNumber)

cardano-cli transaction build \
    --babbage-era \
    --testnet-magic ${TESTNET_MAGIC} \
    --tx-in $UTXO_IN \
    --tx-in-collateral $UTXO_IN \
    --tx-in $UTXO_IN_REQ \
    --spending-tx-in-reference $CC_REF_UTXO \
    --spending-plutus-script-v2 \
    --spending-reference-tx-in-inline-datum-present \
    --spending-reference-tx-in-redeemer-file $REDEEMER_PATH/remove-request.json \
    --tx-out $(cat $WALLET_PATH/$USER.addr)+5000000 \
    --change-address $(cat $WALLET_PATH/$USER.addr) \
    --invalid-before "$nowSlotNumber" \
    --required-signer-hash $USER_PKH \
    --out-file $raw 

cardano-cli transaction sign \
    --testnet-magic ${TESTNET_MAGIC} \
    --tx-body-file $raw \
    --out-file $signed \
    --signing-key-file $WALLET_PATH/$USER.skey

cardano-cli transaction submit \
    --testnet-magic ${TESTNET_MAGIC} \
    --tx-file $signed

tx_submitted $signed $(cat $Validator_Path/threatDatabaseRequest$INDEX.addr)