#!/bin/bash

source env.sh 


USER=$1

UTXO_IN=$(get_address_biggest_lovelace $(cat ../cardano-private-testnet-setup/private-testnet/addresses/utxo1.addr))

cardano-cli transaction build \
    --testnet-magic ${TESTNET_MAGIC} \
    --tx-in ${UTXO_IN} \
    --change-address $(cat ../cardano-private-testnet-setup/private-testnet/addresses/utxo1.addr) \
    --tx-out $(cat $WALLET_PATH/$USER.addr)+10000000000 \
    --out-file $TX_PATH/tx.raw

cardano-cli transaction sign \
    --tx-body-file $TX_PATH/tx.raw \
    --signing-key-file ../cardano-private-testnet-setup/private-testnet/utxo-keys/utxo1.skey \
    --testnet-magic ${TESTNET_MAGIC} \
    --out-file $TX_PATH/tx.signed

cardano-cli transaction submit \
    --testnet-magic ${TESTNET_MAGIC} \
    --tx-file $TX_PATH/tx.signed
