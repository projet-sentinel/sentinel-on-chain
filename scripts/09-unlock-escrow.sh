#!/bin/bash

source env.sh

raw="$TX_PATH/escrow-unlock.raw"
signed="$TX_PATH/escrow-unlock.signed"

USER=$1
DB_INDEX=$2

DB_INDEX_HEX=$(printf '%s' "$DB_INDEX" | xxd -p)
TDAT_CS="$Policy_Path/tdat$DB_INDEX.cs"
USER_PKH=$(cardano-cli address key-hash --payment-verification-key-file $WALLET_PATH/$USER.vkey)

UTXO_IN=$(get_address_biggest_lovelace $(cat $WALLET_PATH/$USER.addr))
UTXO_IN_ESCROW=$(get_address_biggest_lovelace $(cat $Validator_Path/escrow.addr))
AM=$(get_UTxO_lovelace_amount $(cat $Validator_Path/escrow.addr) $UTXO_IN_ESCROW)

THREAT_TOKEN_REF_UTXO=$(get_UTxO_by_token $(cat $Validator_Path/threatDatabase$DB_INDEX.addr) "$(cat $TDAT_CS).$DB_INDEX_HEX")

cabal run write-data Redeemer Unit $REDEEMER_PATH/unit.json

cardano-cli conway transaction build \
    --testnet-magic ${TESTNET_MAGIC} \
    --tx-in $UTXO_IN \
    --tx-in-collateral $UTXO_IN \
    --read-only-tx-in-reference $THREAT_TOKEN_REF_UTXO \
    --tx-in $UTXO_IN_ESCROW \
    --tx-in-script-file $Validator_Path/escrow.plutus \
    --tx-in-inline-datum-present \
    --tx-in-redeemer-file $REDEEMER_PATH/unit.json \
    --tx-out $(cat $WALLET_PATH/$USER.addr)+$AM \
    --change-address $(cat $WALLET_PATH/$USER.addr) \
    --required-signer-hash $USER_PKH \
    --out-file $raw

cardano-cli conway transaction sign \
    --testnet-magic ${TESTNET_MAGIC} \
    --tx-body-file $raw \
    --out-file $signed \
    --signing-key-file $WALLET_PATH/$USER.skey 

cardano-cli conway transaction submit \
    --testnet-magic ${TESTNET_MAGIC} \
    --tx-file $signed

tx_submitted $signed $(cat $WALLET_PATH/$USER.addr)