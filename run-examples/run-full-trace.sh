#!/bin/bash

source run-examples-utils.sh
cd ../scripts
source env.sh

printf "Start private testnet..."

run_node

BOARD_MEMBER1="boardmember1"
BOARD_MEMBER2="boardmember2"
BOARD_MEMBER3="boardmember3"
BOARD_MEMBER4="boardmember4"
BOARD_MEMBER5="boardmember5"

DEBUGGER="debugger"
TREASURY="treasury"
ORACLE_ADMIN="admin1"
SENDER="sender"
RECEIVER="receiver"
BAD="bad-user"

REF_SCR_WALLET="ref-scr-wallet"

printfGreen "Create wallets..."

./create-user-stake.sh $BOARD_MEMBER1
./create-user-stake.sh $BOARD_MEMBER2
./create-user-stake.sh $BOARD_MEMBER3
./create-user-stake.sh $BOARD_MEMBER4
./create-user-stake.sh $BOARD_MEMBER5
./create-user-stake.sh $DEBUGGER
./create-user-stake.sh $REF_SCR_WALLET
./create-user-stake.sh $TREASURY
./create-user-stake.sh $ORACLE_ADMIN
./create-user-stake.sh $SENDER
./create-user-stake.sh $RECEIVER
./create-user-stake.sh $BAD

wait_for_node
echo $CARDANO_NODE_SOCKET_PATH

UTXO_IN=$(get_address_biggest_lovelace $(cat ../cardano-private-testnet-setup/private-testnet/addresses/utxo1.addr))

##### fund wallets
printfGreen "Fund wallets..."
cardano-cli transaction build \
    --testnet-magic ${TESTNET_MAGIC} \
    --tx-in ${UTXO_IN} \
    --change-address $(cat ../cardano-private-testnet-setup/private-testnet/addresses/utxo1.addr) \
    --tx-out $(cat $WALLET_PATH/$BOARD_MEMBER1.addr)+200000000 \
    --tx-out $(cat $WALLET_PATH/$BOARD_MEMBER2.addr)+20000000 \
    --tx-out $(cat $WALLET_PATH/$BOARD_MEMBER3.addr)+20000000 \
    --tx-out $(cat $WALLET_PATH/$BOARD_MEMBER4.addr)+20000000 \
    --tx-out $(cat $WALLET_PATH/$BOARD_MEMBER5.addr)+20000000 \
    --tx-out $(cat $WALLET_PATH/$DEBUGGER.addr)+100000000 \
    --tx-out $(cat $WALLET_PATH/$ORACLE_ADMIN.addr)+100000000 \
    --tx-out $(cat $WALLET_PATH/$SENDER.addr)+50000000 \
    --tx-out $(cat $WALLET_PATH/$RECEIVER.addr)+50000000 \
    --out-file $TX_PATH/tx.raw

cardano-cli transaction sign \
    --tx-body-file $TX_PATH/tx.raw \
    --signing-key-file ../cardano-private-testnet-setup/private-testnet/utxo-keys/utxo1.skey \
    --testnet-magic ${TESTNET_MAGIC} \
    --out-file $TX_PATH/tx.signed

cardano-cli transaction submit \
    --testnet-magic ${TESTNET_MAGIC} \
    --tx-file $TX_PATH/tx.signed

tx_submitted $TX_PATH/tx.signed $(cat $WALLET_PATH/$BOARD_MEMBER1.addr)

balanceUser $BOARD_MEMBER1
balanceUser $BOARD_MEMBER2
balanceUser $BOARD_MEMBER3
balanceUser $BOARD_MEMBER4
balanceUser $BOARD_MEMBER5 
balanceUser $DEBUGGER
balanceUser $ORACLE_ADMIN

printfGreen "We created and funded 5 board members, 1 debugger and 1 oracle admin."
printfGreen "We also created a treasury and a reference script adddress which do not need funds."

#pressContinue

##### create cat
printfGreen "create cat ref..."
./02-create-cat.sh $DEBUGGER $REF_SCR_WALLET

balanceUser $REF_SCR_WALLET
printfGreen "We created a reference script utxo which references the controller authentication policy."
#pressContinue

##### mint board member auth tokens
printfGreen "create bmats..."
./03-create-bmat.sh $BOARD_MEMBER1 & 
./03-create-bmat.sh $BOARD_MEMBER2 &
./03-create-bmat.sh $BOARD_MEMBER3 &
./03-create-bmat.sh $BOARD_MEMBER4 &
./03-create-bmat.sh $BOARD_MEMBER5

wait

balanceValidator controller

printfGreen "Every board member minted an authentication token and locked it at the contract controller validator, to verify his ownership in the system."
printfGreen "Each utxo has a datum attached where board members store their public key hashes. For simplicity just one but could be more than that."
cat $DATUM_PATH/board-member-dat-1.json | jq
#pressContinue

##### create controller utxo
printfGreen "create controller..."
./04-create-controller.sh $BOARD_MEMBER1 $REF_SCR_WALLET

balanceValidator controller
printfGreen "Now we created a references utxo for the controller validator and finished the initialization of the administrativ part."
cat $DATUM_PATH/contracts-controller-ref-script-dat.json | jq
printfGreen "Every board member has to sign and add his board member auth token to the datum."

#pressContinue

##### create oracle ref utxo
printfGreen "create oracle ref scripts..."
./05addr-create-oracle-ref.sh $BOARD_MEMBER1 $REF_SCR_WALLET 1 $DEBUGGER $ORACLE_ADMIN $TREASURY

balanceValidator controller
printfGreen "Now we registered the first oracle by creating two reference utxo which were signed by all board member."
cat $DATUM_PATH/td-ref-script-dat1.json | jq
printfGreen "The first reference utxo is for the threat databse validator."
printfGreen "In the datum we have an index which is to differenciate between instances, a list of public key hashes who are allowed \n
to update threat data and an address where funds have to go."
cat $DATUM_PATH/tdat-ref-script-dat1.json | jq
printfGreen "The second reference utxo is for the threat data base token which verifies valid threat data utxos."
printfGreen "In the datum we again define the index, a list of allowed public key hashes who are allowed to mint new threat database \n
tokens but instead of a treasury address we have the threat database validator address where we will lock these tokens."

#pressContinue

##### mint tdat
printfGreen "mint tdat..."
./06addr-mint-tdat.sh $ORACLE_ADMIN 1 $BAD 5

balanceValidator threatDatabase1
printfGreen "Now we minted our first threat data utxo, verified by it's currency symbol $(cat $Policy_Path/tdat1.cs) and locked it \n
at the threat database validator."
cat $DATUM_PATH/threat-database-dat1.json | jq
printfGreen "In the datum we can find the threat data. First we have a tag to differenciate between data, second the threat score, \n
third some sample threat data (a list of addresses) and fourth the time it was last updated (POSIX time stamp)." 

#pressContinue

./08-lock-escrow.sh $SENDER $RECEIVER 10000000 1

balanceValidator escrow

printfGreen "A client locked 10 Ada in an escrow contract."

#pressContinue

./09-unlock-escrow.sh $RECEIVER 1

balanceUser $RECEIVER

printfGreen "update threat score..."
./07a-update-threat-score.sh $ORACLE_ADMIN 1 9

balanceValidator threatDatabase1
cat $DATUM_PATH/threat-database-dat1.json | jq
printfGreen "In this transaction we updated the threat score."

#pressContinue

printfGreen "remove threat score..."
./07b-remove-threat-score.sh $ORACLE_ADMIN 1 $TREASURY

balanceValidator threatDatabase1
cat $DATUM_PATH/threat-database-dat1.json | jq
printfGreen "In this transaction we removed the threat score and sent the funds locked with the token to the treasury."
balanceUser $TREASURY