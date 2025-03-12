#!/bin/bash

source env.sh

raw="$TX_PATH/escrow-lock.raw"
signed="$TX_PATH/escrow-lock.signed"

USER=$1
RECEIVER=$2
AM=$3
DB_INDEX=$4

TDAT_CS="$Policy_Path/tdat$DB_INDEX.cs"

UTXO_IN=$(get_address_biggest_lovelace $(cat $WALLET_PATH/$USER.addr))

cabal run write-data Validator Escrow $Validator_Path/escrow.plutus $(cat $TDAT_CS) $(cat $Validator_Path/threatDatabase$DB_INDEX.addr)
cardano-cli address build --testnet-magic ${TESTNET_MAGIC} --payment-script-file $Validator_Path/escrow.plutus --out-file $Validator_Path/escrow.addr

cabal run write-data Datum Escrow $DATUM_PATH/escrow-dat-$USER.json $(cat $WALLET_PATH/$USER.addr) $(cat $WALLET_PATH/$RECEIVER.addr)

cardano-cli conway transaction build \
    --testnet-magic ${TESTNET_MAGIC} \
    --tx-in $UTXO_IN \
    --tx-in-collateral $UTXO_IN \
    --tx-out $(cat $Validator_Path/escrow.addr)+$AM \
    --tx-out-inline-datum-file $DATUM_PATH/escrow-dat-$USER.json \
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

tx_submitted $signed $(cat $Validator_Path/escrow.addr)