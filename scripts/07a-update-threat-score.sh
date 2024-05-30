#!/bin/bash

source env.sh

USER=$1
INDEX=$2
THREAT_SCORE=$3
INDEX_HEX=$(printf '%s' "$INDEX" | xxd -p)

INDEX_DATA_TAG=1

if [ $4 ]; then
    INDEX_DATA_TAG=$4
fi

raw="$TX_PATH/update-score-$INDEX.raw"
signed="$TX_PATH/update-score-$INDEX.signed"

USER_PKH=$(cardano-cli address key-hash --payment-verification-key-file $WALLET_PATH/$USER.vkey)

TDAT_CS="$Policy_Path/tdat$INDEX.cs"

UTXO_IN=$(get_address_biggest_lovelace $(cat $WALLET_PATH/$USER.addr))
TDAT_UTXO_IN1=$(get_UTxO_by_token $(cat $Validator_Path/threatDatabase$INDEX.addr) "$(cat $TDAT_CS).$INDEX_HEX")

TDAT_UTXO_IN=$(get_database_utxo $(cat $Validator_Path/threatDatabase$INDEX.addr) $INDEX_DATA_TAG)

CC_REF_UTXO=$(get_UTxO_by_token $(cat $CONTROLLER_ADDR) "$(cat $CAT_CS).$(cat $Validator_Path/threatDatabase$INDEX.vh)")

nowSlotNumber=$(get_current_slot_number)
submissionTime=$((nowSlotNumber + 100))

cabal run write-data Redeemer ThreatDatabase UpdateThreatData $REDEEMER_PATH/update-threat-data-$INDEX.json

cabal run write-data Datum ThreatDatabaseAddrListDatum $DATUM_PATH/threat-database-dat$INDEX.json $INDEX_DATA_TAG $THREAT_SCORE $(($(get_current_POSIX_time) + 3600000)) $(cat $WALLET_PATH/$USER.addr)

sleep 2

cardano-cli transaction build \
    --babbage-era \
    --testnet-magic ${TESTNET_MAGIC} \
    --tx-in $UTXO_IN \
    --tx-in-collateral $UTXO_IN \
    --tx-in $TDAT_UTXO_IN \
    --spending-tx-in-reference $CC_REF_UTXO \
    --spending-plutus-script-v2 \
    --spending-reference-tx-in-inline-datum-present \
    --spending-reference-tx-in-redeemer-file $REDEEMER_PATH/update-threat-data-$INDEX.json \
    --tx-out $(cat $Validator_Path/threatDatabase$INDEX.addr)+3000000+"1 $(cat $TDAT_CS).$INDEX_HEX" \
    --tx-out-inline-datum-file $DATUM_PATH/threat-database-dat$INDEX.json \
    --change-address $(cat $WALLET_PATH/$USER.addr) \
    --invalid-hereafter "$submissionTime" \
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

tx_submitted $signed $(cat $Validator_Path/threatDatabase$INDEX.addr)