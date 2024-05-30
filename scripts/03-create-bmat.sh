#!/bin/bash

source env.sh

USER=$1
MEM_ID=${USER: -1}

raw="$TX_PATH/init-board-$MEM_ID.raw"
signed="$TX_PATH/init-board-$MEM_ID.signed"

USER_PKH=$(cardano-cli address key-hash --payment-verification-key-file $WALLET_PATH/$USER.vkey)

BMAT_Policy="$Policy_Path/bmat-$MEM_ID.plutus"
BMAT_CS="$Policy_Path/bmat-$MEM_ID.cs"

B_MEM_DAT="$DATUM_PATH/board-member-dat-$MEM_ID.json"

UTXO_IN=$(get_address_biggest_lovelace $(cat $WALLET_PATH/$USER.addr))

IFS='#'

read -a strarr <<< "$UTXO_IN"
txHash=${strarr[0]}
txId=${strarr[1]}

cabal run write-data Policy BMAT $BMAT_Policy $txHash $txId $(cat $WALLET_PATH/$USER.addr)
cardano-cli transaction policyid --script-file $BMAT_Policy > $BMAT_CS

cabal run write-data Redeemer Unit $REDEEMER_PATH/unit.json

cabal run write-data Datum Controller BoardMemberDatum $B_MEM_DAT $(cat $WALLET_PATH/$USER.addr)

cardano-cli transaction build \
    --babbage-era \
    --testnet-magic ${TESTNET_MAGIC} \
    --tx-in $txHash#$txId \
    --tx-in-collateral $txHash#$txId \
    --mint "1 $(cat $BMAT_CS).""" \
    --mint-script-file $BMAT_Policy \
    --mint-redeemer-file $REDEEMER_PATH/unit.json \
    --tx-out $(cat $CONTROLLER_ADDR)+3000000+"1 $(cat $BMAT_CS).""" \
    --tx-out-inline-datum-file $B_MEM_DAT \
    --change-address $(cat $WALLET_PATH/$USER.addr) \
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

tx_submitted $signed $(cat $CONTROLLER_ADDR)