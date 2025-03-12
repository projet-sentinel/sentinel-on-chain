#!/bin/bash

source env.sh

USER=$1
INDEX=$2
TREASURY=$3
INDEX_HEX=$(printf '%s' "$INDEX" | xxd -p)

raw="$TX_PATH/burn-tdat-$INDEX.raw"
signed="$TX_PATH/burn-tdat-$INDEX.signed"

USER_PKH=$(cardano-cli address key-hash --payment-verification-key-file $WALLET_PATH/$USER.vkey)

TDAT_CS="$Policy_Path/tdat$INDEX.cs"

UTXO_IN=$(get_address_biggest_lovelace $(cat $WALLET_PATH/$USER.addr))
TDAT_UTXO_IN=$(get_UTxO_by_token $(cat $Validator_Path/threatDatabase$INDEX.addr) "$(cat $TDAT_CS).$INDEX_HEX")
TDAT_ADA_AM=$(get_UTxO_lovelace_amount $(cat $Validator_Path/threatDatabase$INDEX.addr) "$TDAT_UTXO_IN")

CC_REF_UTXO_VALIDATOR=$(get_UTxO_by_token $(cat $CONTROLLER_ADDR) "$(cat $CAT_CS).$(cat $Validator_Path/threatDatabase$INDEX.vh)")
CC_REF_UTXO_POLICY=$(get_UTxO_by_token $(cat $CONTROLLER_ADDR) "$(cat $CAT_CS).$(cat $TDAT_CS)")

sleep 1

cabal run write-data Redeemer ThreatDatabase RemoveThreatData $REDEEMER_PATH/remove-threat-data-$INDEX.json
cabal run write-data Redeemer TDAT BurnTDAT $REDEEMER_PATH/burn-tdat-$INDEX.json

cardano-cli conway transaction build \
    --testnet-magic ${TESTNET_MAGIC} \
    --tx-in $UTXO_IN \
    --tx-in-collateral $UTXO_IN \
    --tx-in $TDAT_UTXO_IN \
    --spending-tx-in-reference $CC_REF_UTXO_VALIDATOR \
    --spending-plutus-script-v2 \
    --spending-reference-tx-in-inline-datum-present \
    --spending-reference-tx-in-redeemer-file $REDEEMER_PATH/remove-threat-data-$INDEX.json \
    --mint "-1 $(cat $TDAT_CS).$INDEX_HEX" \
    --mint-tx-in-reference $CC_REF_UTXO_POLICY \
    --mint-plutus-script-v2 \
    --mint-reference-tx-in-redeemer-file $REDEEMER_PATH/burn-tdat-$INDEX.json \
    --policy-id $(cat $TDAT_CS) \
    --tx-out $(cat $WALLET_PATH/$TREASURY.addr)+$TDAT_ADA_AM \
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

tx_submitted $signed $(cat $WALLET_PATH/$TREASURY.addr)