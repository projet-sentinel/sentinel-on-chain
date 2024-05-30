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
BAD="bad-user"

ORACLE_ADMIN="admin"
AM_ADMINS=1
AM_TAGS=3

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
./create-user-stake.sh $BAD

FUNDA_ADMINS_TXOUTS=""

for (( i=1; i<=$AM_ADMINS; i++ ))
do  
    ./create-user-stake.sh $ORACLE_ADMIN$i
    FUND_ADMINS_TXOUTS+=" --tx-out $(cat $WALLET_PATH/$ORACLE_ADMIN$i.addr)+100000000"
done

wait_for_node

UTXO_IN=$(get_address_biggest_lovelace $(cat ../cardano-private-testnet-setup/private-testnet/addresses/utxo1.addr))

##### fund wallets
printfGreen "Fund wallets..."

fund_tx_building=$(cardano-cli transaction build \
    --testnet-magic ${TESTNET_MAGIC} \
    --tx-in ${UTXO_IN} \
    --change-address $(cat ../cardano-private-testnet-setup/private-testnet/addresses/utxo1.addr) \
    --tx-out $(cat $WALLET_PATH/$BOARD_MEMBER1.addr)+2000000000 \
    --tx-out $(cat $WALLET_PATH/$BOARD_MEMBER2.addr)+20000000 \
    --tx-out $(cat $WALLET_PATH/$BOARD_MEMBER3.addr)+20000000 \
    --tx-out $(cat $WALLET_PATH/$BOARD_MEMBER4.addr)+20000000 \
    --tx-out $(cat $WALLET_PATH/$BOARD_MEMBER5.addr)+20000000 \
    --tx-out $(cat $WALLET_PATH/$DEBUGGER.addr)+100000000 \
    $(echo $FUND_ADMINS_TXOUTS) \
    --out-file $TX_PATH/tx.raw
)

echo $fund_tx_building

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

for (( i=1; i<=$AM_ADMINS; i++ ))
do  
    balanceUser $ORACLE_ADMIN$i
done

printfGreen "We created and funded 5 board members, 1 debugger and $AM_ADMINS oracle admins."
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

##### create oracle ref utxo
printfGreen "create oracle ref scripts..."
for (( i=1; i<=$AM_ADMINS; i++ ))
do  
    ./05addr-create-oracle-ref.sh $BOARD_MEMBER1 $REF_SCR_WALLET $i $DEBUGGER $ORACLE_ADMIN$i $TREASURY
done

balanceValidator controller
printfGreen "Now we registered $(( $AM_ADMINS * 2 )) reference scripts where each two are for one oralce admin."
cat $DATUM_PATH/td-ref-script-dat1.json | jq
printfGreen "The first reference utxo is for the threat databse validator."
printfGreen "In the datum we have an index which is to differenciate between instances, a list of public key hashes who are allowed \n
to update threat data and an address where funds have to go."
cat $DATUM_PATH/tdat-ref-script-dat1.json | jq
printfGreen "The second reference utxo is for the threat data base token which verifies valid threat data utxos."
printfGreen "In the datum we again define the index, a list of allowed public key hashes who are allowed to mint new threat database \n
tokens but instead of a treasury address we have the threat database validator address where we will lock these tokens."

#pressContinue

printfGreen "mint tdat..."
for (( i=1; i<=$AM_TAGS; i++ ))
do 
    ./06addr-mint-tdat.sh $ORACLE_ADMIN"1" 1 $BAD 5 $i
done

for (( i=1; i<=$AM_ADMINS; i++ ))
do 
    balanceValidator threatDatabase$i
done

printfGreen "Now we minted our first threat data utxos, where each is verified with a different currency symbol \n
(threat database authentication token policy) at a different validator (threat database validator)."
cat $DATUM_PATH/threat-database-dat1.json | jq
printfGreen "In the datum we can find the threat data. First we have a tag to differenciate between data, second the threat score, \n
third some sample threat data (a list of addresses) and fourth the time it was last updated (POSIX time stamp)."

#pressContinue

printfGreen "update threat score..."
for (( i=1; i<=$AM_TAGS; i++ ))
do 
    ./07a-update-threat-score.sh $ORACLE_ADMIN"1" 1 6 $i
done

balanceValidator threatDatabase1
cat $DATUM_PATH/threat-database-dat1.json | jq
printfGreen "In this transaction we updated the threat scores."

#pressContinue

printfGreen "remove threat score..."
for (( i=1; i<=$AM_TAGS; i++ ))
do 
    ./07b-remove-threat-score.sh $ORACLE_ADMIN"1" 1 $TREASURY
done

balanceValidator threatDatabase1
cat $DATUM_PATH/threat-database-dat1.json | jq
printfGreen "In this transaction we removed all threat utxos and sent the funds locked with the tokens to the treasury."
balanceUser $TREASURY
