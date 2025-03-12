#!/bin/bash

source env.sh

USER=$1
INDEX=$2
BAD_WALLET_NAME=$3
THREAT_SCORE=$4
INDEX_HEX=$(printf '%s' "$INDEX" | xxd -p)

INDEX_DATA_TAG=1

if [ $5 ]; then
    INDEX_DATA_TAG=$5
fi

raw="$TX_PATH/mint-tdat-sh-$INDEX.raw"
signed="$TX_PATH/mint-tdat-sh-$INDEX.signed"

USER_PKH=$(cardano-cli address key-hash --payment-verification-key-file $WALLET_PATH/$USER.vkey)

TDAT_Policy="$Policy_Path/tdat$INDEX.plutus"
TDAT_CS="$Policy_Path/tdat$INDEX.cs"

UTXO_IN=$(get_address_biggest_lovelace $(cat $WALLET_PATH/$USER.addr))

CC_REF_UTXO=$(get_UTxO_by_token $(cat $CONTROLLER_ADDR) "$(cat $CAT_CS).$(cat $TDAT_CS)")

sleep 1

nowSlotNumber=$(get_current_slot_number)
submissionTime=$((nowSlotNumber + 60))

cabal run write-data Redeemer TDAT MintTDAT $REDEEMER_PATH/mint-tdat-$INDEX.json

cabal run write-data Datum ThreatDatabaseAddrListDatum $DATUM_PATH/threat-database-dat$INDEX.json $INDEX_DATA_TAG $THREAT_SCORE $(($(get_current_POSIX_time) + 3600000)) $(cat $WALLET_PATH/$BAD_WALLET_NAME.addr)

cardano-cli conway transaction build \
    --testnet-magic ${TESTNET_MAGIC} \
    --tx-in $UTXO_IN \
    --tx-in-collateral $UTXO_IN \
    --mint "1 $(cat $TDAT_CS).$INDEX_HEX" \
    --mint-tx-in-reference $CC_REF_UTXO \
    --mint-plutus-script-v2 \
    --mint-reference-tx-in-redeemer-file $REDEEMER_PATH/mint-tdat-$INDEX.json \
    --policy-id $(cat $TDAT_CS) \
    --tx-out $(cat $Validator_Path/threatDatabase$INDEX.addr)+3000000+"1 $(cat $TDAT_CS).$INDEX_HEX" \
    --tx-out-inline-datum-file $DATUM_PATH/threat-database-dat$INDEX.json \
    --change-address $(cat $WALLET_PATH/$USER.addr) \
    --invalid-hereafter "$submissionTime" \
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

tx_submitted $signed $(cat $Validator_Path/threatDatabase$INDEX.addr)