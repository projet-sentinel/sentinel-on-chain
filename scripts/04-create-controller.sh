#!/bin/bash

source env.sh

USER=$1
MEM_ID=${USER: -1}
REF_SCR_WALLET_NAME=$2

raw="$TX_PATH/controller-ref-$MEM_ID.raw"
signed="$TX_PATH/controller-ref-$MEM_ID.signed"

UTXO_IN=$(get_address_biggest_lovelace $(cat $WALLET_PATH/$USER.addr))
UTXO_IN_REF_SCR=$(get_address_biggest_lovelace $(cat $WALLET_PATH/$REF_SCR_WALLET_NAME.addr))

BMAT_CS1="$Policy_Path/bmat-1.cs"
BMAT_CS2="$Policy_Path/bmat-2.cs"
BMAT_CS3="$Policy_Path/bmat-3.cs"
BMAT_CS4="$Policy_Path/bmat-4.cs"
BMAT_CS5="$Policy_Path/bmat-5.cs"

UTXO_BMAT1=$(get_UTxO_by_token $(cat $CONTROLLER_ADDR) "$(cat $BMAT_CS1)" "")
UTXO_BMAT2=$(get_UTxO_by_token $(cat $CONTROLLER_ADDR) "$(cat $BMAT_CS2)" "")
UTXO_BMAT3=$(get_UTxO_by_token $(cat $CONTROLLER_ADDR) "$(cat $BMAT_CS3)" "")
UTXO_BMAT4=$(get_UTxO_by_token $(cat $CONTROLLER_ADDR) "$(cat $BMAT_CS4)" "")
UTXO_BMAT5=$(get_UTxO_by_token $(cat $CONTROLLER_ADDR) "$(cat $BMAT_CS5)" "")

BMAT_PKH1=$(cardano-cli address key-hash --payment-verification-key-file $WALLET_PATH/boardmember1.vkey)
BMAT_PKH2=$(cardano-cli address key-hash --payment-verification-key-file $WALLET_PATH/boardmember2.vkey)
BMAT_PKH3=$(cardano-cli address key-hash --payment-verification-key-file $WALLET_PATH/boardmember3.vkey)
BMAT_PKH4=$(cardano-cli address key-hash --payment-verification-key-file $WALLET_PATH/boardmember4.vkey)
BMAT_PKH5=$(cardano-cli address key-hash --payment-verification-key-file $WALLET_PATH/boardmember5.vkey)

nowSlotNumber=$(get_current_slot_number)
submissionTime=$((nowSlotNumber + 100))

sleep 1

cabal run write-data Redeemer CAT SentinelBoardInception $REDEEMER_PATH/sentinelBoardInception-red.json

cabal run write-data Datum Controller ContractsControllerRefScriptDatum $DATUM_PATH/contracts-controller-ref-script-dat.json $(($(get_current_POSIX_time) + 31539600000)) $(cat $BMAT_CS1) $(cat $BMAT_CS2) $(cat $BMAT_CS3) $(cat $BMAT_CS4) $(cat $BMAT_CS5)

cardano-cli conway transaction build \
    --testnet-magic ${TESTNET_MAGIC} \
    --tx-in $UTXO_IN \
    --tx-in-collateral $UTXO_IN \
    --read-only-tx-in-reference $UTXO_BMAT1 \
    --read-only-tx-in-reference $UTXO_BMAT2 \
    --read-only-tx-in-reference $UTXO_BMAT3 \
    --read-only-tx-in-reference $UTXO_BMAT4 \
    --read-only-tx-in-reference $UTXO_BMAT5 \
    --mint "1 $(cat $CAT_CS).""" \
    --mint-tx-in-reference $UTXO_IN_REF_SCR \
    --mint-plutus-script-v2 \
    --mint-reference-tx-in-redeemer-file $REDEEMER_PATH/sentinelBoardInception-red.json \
    --policy-id $(cat $CAT_CS) \
    --invalid-hereafter "$submissionTime" \
    --required-signer-hash $BMAT_PKH1 \
    --required-signer-hash $BMAT_PKH2 \
    --required-signer-hash $BMAT_PKH3 \
    --required-signer-hash $BMAT_PKH4 \
    --required-signer-hash $BMAT_PKH5 \
    --tx-out $(cat $CONTROLLER_ADDR)+21330190+"1 $(cat $CAT_CS).""" \
    --tx-out-inline-datum-file $DATUM_PATH/contracts-controller-ref-script-dat.json \
    --tx-out-reference-script-file $CONTROLLER \
    --change-address $(cat $WALLET_PATH/$USER.addr) \
    --out-file $raw

cardano-cli conway transaction sign \
    --testnet-magic ${TESTNET_MAGIC} \
    --tx-body-file $raw \
    --out-file $signed \
    --signing-key-file $WALLET_PATH/$USER.skey \
    --signing-key-file $WALLET_PATH/boardmember1.skey \
    --signing-key-file $WALLET_PATH/boardmember2.skey \
    --signing-key-file $WALLET_PATH/boardmember3.skey \
    --signing-key-file $WALLET_PATH/boardmember4.skey \
    --signing-key-file $WALLET_PATH/boardmember5.skey \

cardano-cli conway transaction submit \
    --testnet-magic ${TESTNET_MAGIC} \
    --tx-file $signed

tx_submitted $signed $(cat $CONTROLLER_ADDR)