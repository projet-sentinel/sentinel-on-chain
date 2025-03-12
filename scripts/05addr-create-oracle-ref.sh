#!/bin/bash

source env.sh

USER=$1
REF_SCR_WALLET_NAME=$2
INDEX=$3
DEB=$4
ADMIN_NAME=$5
TREASURY_NAME=$6

raw="$TX_PATH/oracle-ref-addr-$INDEX.raw"
signed="$TX_PATH/oracle-ref-addr-$INDEX.signed"

UTXO_IN=$(get_address_biggest_lovelace $(cat $WALLET_PATH/$USER.addr))
UTXO_IN_REF_SCR=$(get_address_biggest_lovelace $(cat $WALLET_PATH/$REF_SCR_WALLET_NAME.addr))

BMAT_CS1="$Policy_Path/bmat-1.cs"
BMAT_CS2="$Policy_Path/bmat-2.cs"
BMAT_CS3="$Policy_Path/bmat-3.cs"
BMAT_CS4="$Policy_Path/bmat-4.cs"
BMAT_CS5="$Policy_Path/bmat-5.cs"

CC_REF_UTXO=$(get_UTxO_by_token $(cat $CONTROLLER_ADDR) "$(cat $CAT_CS)")
sleep 1
UTXO_BMAT1=$(get_UTxO_by_token $(cat $CONTROLLER_ADDR) "$(cat $BMAT_CS1)")
sleep 1
UTXO_BMAT2=$(get_UTxO_by_token $(cat $CONTROLLER_ADDR) "$(cat $BMAT_CS2)")
sleep 1
UTXO_BMAT3=$(get_UTxO_by_token $(cat $CONTROLLER_ADDR) "$(cat $BMAT_CS3)")
sleep 1
UTXO_BMAT4=$(get_UTxO_by_token $(cat $CONTROLLER_ADDR) "$(cat $BMAT_CS4)")
sleep 1
UTXO_BMAT5=$(get_UTxO_by_token $(cat $CONTROLLER_ADDR) "$(cat $BMAT_CS5)")
sleep 1

BMAT_PKH1=$(cardano-cli address key-hash --payment-verification-key-file $WALLET_PATH/boardmember1.vkey)
BMAT_PKH2=$(cardano-cli address key-hash --payment-verification-key-file $WALLET_PATH/boardmember2.vkey)
BMAT_PKH3=$(cardano-cli address key-hash --payment-verification-key-file $WALLET_PATH/boardmember3.vkey)
BMAT_PKH4=$(cardano-cli address key-hash --payment-verification-key-file $WALLET_PATH/boardmember4.vkey)
BMAT_PKH5=$(cardano-cli address key-hash --payment-verification-key-file $WALLET_PATH/boardmember5.vkey)

nowSlotNumber=$(get_current_slot_number)
submissionTime=$((nowSlotNumber + 150))

cabal run write-data Policy TDATAddrList $Policy_Path/tdat$INDEX.plutus $INDEX $(cat $CONTROLLER_VH) $(cat $CAT_CS) $(cat $WALLET_PATH/$DEB.addr)
cardano-cli conway transaction policyid --script-file $Policy_Path/tdat$INDEX.plutus > $Policy_Path/tdat$INDEX.cs

cabal run write-data Validator ThreatDatabaseAddrList $Validator_Path/threatDatabase$INDEX.plutus $INDEX $(cat $CONTROLLER_VH) $(cat $CAT_CS) $(cat $Policy_Path/tdat$INDEX.cs) $(cat $WALLET_PATH/$DEB.addr)
cardano-cli address build --testnet-magic ${TESTNET_MAGIC} --payment-script-file $Validator_Path/threatDatabase$INDEX.plutus --out-file $Validator_Path/threatDatabase$INDEX.addr
cardano-cli conway transaction policyid --script-file $Validator_Path/threatDatabase$INDEX.plutus > $Validator_Path/threatDatabase$INDEX.vh

cabal run write-data Validator TDR $Validator_Path/threatDatabaseRequest$INDEX.plutus $INDEX $(cat $CONTROLLER_VH) $(cat $CAT_CS) $(cat $Policy_Path/tdat$INDEX.cs) $(cat $WALLET_PATH/$DEB.addr)
cardano-cli address build --testnet-magic ${TESTNET_MAGIC} --payment-script-file $Validator_Path/threatDatabaseRequest$INDEX.plutus --out-file $Validator_Path/threatDatabaseRequest$INDEX.addr
cardano-cli conway transaction policyid --script-file $Validator_Path/threatDatabaseRequest$INDEX.plutus > $Validator_Path/threatDatabaseRequest$INDEX.vh

cabal run write-data Redeemer CAT SentinelBoardAction $REDEEMER_PATH/sentinelBoardAction-red.json

cabal run write-data Datum ThreatDatabaseRefScriptDatum $DATUM_PATH/threat-database-ref-dat-$INDEX.json $INDEX $(cat $WALLET_PATH/$TREASURY_NAME.addr) $(cat $WALLET_PATH/$ADMIN_NAME.addr)
cabal run write-data Datum TDATRefScriptDatum $DATUM_PATH/tdat-ref-dat-$INDEX.json $INDEX $(cat $Validator_Path/threatDatabase$INDEX.addr) $(cat $WALLET_PATH/$ADMIN_NAME.addr)
cabal run write-data Datum TDRRefScriptDatum $DATUM_PATH/tdr-ref-dat$INDEX.json $INDEX $INDEX $(cat $Validator_Path/threatDatabase$INDEX.vh) 1

cabal run write-data Datum Controller ContractRefScriptDatum ThreatDatabase $DATUM_PATH/td-ref-script-dat$INDEX.json $DATUM_PATH/threat-database-ref-dat-$INDEX.json
cabal run write-data Datum Controller ContractRefScriptDatum TDAT $DATUM_PATH/tdat-ref-script-dat$INDEX.json $DATUM_PATH/tdat-ref-dat-$INDEX.json
cabal run write-data Datum Controller ContractRefScriptDatum TDR $DATUM_PATH/tdr-ref-script-dat$INDEX.json $DATUM_PATH/tdr-ref-dat$INDEX.json

cardano-cli conway transaction build \
    --testnet-magic ${TESTNET_MAGIC} \
    --tx-in $UTXO_IN \
    --tx-in-collateral $UTXO_IN \
    --read-only-tx-in-reference $CC_REF_UTXO \
    --read-only-tx-in-reference $UTXO_BMAT1 \
    --read-only-tx-in-reference $UTXO_BMAT2 \
    --read-only-tx-in-reference $UTXO_BMAT3 \
    --read-only-tx-in-reference $UTXO_BMAT4 \
    --read-only-tx-in-reference $UTXO_BMAT5 \
    --mint "1 $(cat $CAT_CS).$(cat $Validator_Path/threatDatabase$INDEX.vh) + 1 $(cat $CAT_CS).$(cat $Policy_Path/tdat$INDEX.cs) + 1 $(cat $CAT_CS).$(cat $Validator_Path/threatDatabaseRequest$INDEX.vh)" \
    --mint-tx-in-reference $UTXO_IN_REF_SCR \
    --mint-plutus-script-v2 \
    --mint-reference-tx-in-redeemer-file $REDEEMER_PATH/sentinelBoardAction-red.json \
    --policy-id $(cat $CAT_CS) \
    --invalid-hereafter "$submissionTime" \
    --required-signer-hash $BMAT_PKH1 \
    --required-signer-hash $BMAT_PKH2 \
    --required-signer-hash $BMAT_PKH3 \
    --required-signer-hash $BMAT_PKH4 \
    --required-signer-hash $BMAT_PKH5 \
    --tx-out $(cat $CONTROLLER_ADDR)+18597650+"1 $(cat $CAT_CS).$(cat $Validator_Path/threatDatabase$INDEX.vh)" \
    --tx-out-inline-datum-file $DATUM_PATH/td-ref-script-dat$INDEX.json \
    --tx-out-reference-script-file $Validator_Path/threatDatabase$INDEX.plutus \
    --tx-out $(cat $CONTROLLER_ADDR)+18106310+"1 $(cat $CAT_CS).$(cat $Policy_Path/tdat$INDEX.cs)" \
    --tx-out-inline-datum-file $DATUM_PATH/tdat-ref-script-dat$INDEX.json \
    --tx-out-reference-script-file $Policy_Path/tdat$INDEX.plutus \
    --tx-out $(cat $CONTROLLER_ADDR)+19416550+"1 $(cat $CAT_CS).$(cat $Validator_Path/threatDatabaseRequest$INDEX.vh)" \
    --tx-out-inline-datum-file $DATUM_PATH/tdr-ref-script-dat$INDEX.json \
    --tx-out-reference-script-file $Validator_Path/threatDatabaseRequest$INDEX.plutus \
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