#!/bin/bash

source env.sh

USER=$1
INDEX=$2
THREAT_SCORE=$3
TREASURY=$4
REQ_ADDR=$5
INDEX_HEX=$(printf '%s' "$INDEX" | xxd -p)

INDEX_DATA_TAG=1

if [ $6 ]; then
    INDEX_DATA_TAG=$6
fi

raw="$TX_PATH/update-score-$INDEX.raw"
signed="$TX_PATH/update-score-$INDEX.signed"

USER_PKH=$(cardano-cli address key-hash --payment-verification-key-file $WALLET_PATH/$USER.vkey)

TDAT_CS="$Policy_Path/tdat$INDEX.cs"

UTXO_IN=$(get_address_biggest_lovelace $(cat $WALLET_PATH/$USER.addr))
TDAT_UTXO_IN1=$(get_UTxO_by_token $(cat $Validator_Path/threatDatabase$INDEX.addr) "$(cat $TDAT_CS).$INDEX_HEX")

TDAT_UTXO_IN=$(get_database_utxo $(cat $Validator_Path/threatDatabase$INDEX.addr) $INDEX_DATA_TAG)
UTXO_IN_REQ=$(get_address_biggest_lovelace $(cat $Validator_Path/threatDatabaseRequest$INDEX.addr))

CC_TD_REF_UTXO=$(get_UTxO_by_token $(cat $CONTROLLER_ADDR) "$(cat $CAT_CS).$(cat $Validator_Path/threatDatabase$INDEX.vh)")
CC_TDR_REF_UTXO=$(get_UTxO_by_token $(cat $CONTROLLER_ADDR) "$(cat $CAT_CS).$(cat $Validator_Path/threatDatabaseRequest$INDEX.vh)")

nowSlotNumber=$(get_current_slot_number)
submissionTime=$((nowSlotNumber + 100))

cabal run write-data Redeemer ThreatDatabase UpdateThreatData $REDEEMER_PATH/update-threat-data-$INDEX.json
cabal run write-data Redeemer TDR AcceptRequest $REDEEMER_PATH/tdr-accept.json

cabal run write-data Datum ThreatDatabaseAddrListDatumAddAddrs $DATUM_PATH/threat-database-dat$INDEX.json $INDEX_DATA_TAG $THREAT_SCORE $(($(get_current_POSIX_time) + 3600000)) $(cat $WALLET_PATH/$REQ_ADDR.addr)

sleep 2


cardano-cli transaction build \
    --babbage-era \
    --testnet-magic ${TESTNET_MAGIC} \
    --tx-in $UTXO_IN \
    --tx-in-collateral $UTXO_IN \
    --tx-in $TDAT_UTXO_IN \
    --spending-tx-in-reference $CC_TD_REF_UTXO \
    --spending-plutus-script-v2 \
    --spending-reference-tx-in-inline-datum-present \
    --spending-reference-tx-in-redeemer-file $REDEEMER_PATH/update-threat-data-$INDEX.json \
    --tx-in $UTXO_IN_REQ \
    --spending-tx-in-reference $CC_TDR_REF_UTXO \
    --spending-plutus-script-v2 \
    --spending-reference-tx-in-inline-datum-present \
    --spending-reference-tx-in-redeemer-file $REDEEMER_PATH/tdr-accept.json \
    --tx-out $(cat $Validator_Path/threatDatabase$INDEX.addr)+3000000+"1 $(cat $TDAT_CS).$INDEX_HEX" \
    --tx-out-inline-datum-file $DATUM_PATH/threat-database-dat$INDEX.json \
    --tx-out $(cat $WALLET_PATH/$TREASURY.addr)+5000000 \
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