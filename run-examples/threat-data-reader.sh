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
SENDER="sender"
RECEIVER="receiver"

ORACLE_ADMIN="admin"
AM_ADMINS=1

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
./create-user-stake.sh $SENDER
./create-user-stake.sh $RECEIVER

FUND_ADMINS_TXOUTS=""

for (( i=1; i<=$AM_ADMINS; i++ ))
do  
    ./create-user-stake.sh $ORACLE_ADMIN$i
    FUND_ADMINS_TXOUTS+=" --tx-out $(cat $WALLET_PATH/$ORACLE_ADMIN$i.addr)+100000000"
done

wait_for_node

UTXO_IN=$(get_address_biggest_lovelace $(cat ../cardano-private-testnet-setup/private-testnet/addresses/utxo1.addr))

##### fund wallets
printfGreen "Fund wallets..."

fund_tx_building=$(cardano-cli conway transaction build \
    --testnet-magic ${TESTNET_MAGIC} \
    --tx-in ${UTXO_IN} \
    --change-address $(cat ../cardano-private-testnet-setup/private-testnet/addresses/utxo1.addr) \
    --tx-out $(cat $WALLET_PATH/$BOARD_MEMBER1.addr)+2000000000 \
    --tx-out $(cat $WALLET_PATH/$BOARD_MEMBER2.addr)+20000000 \
    --tx-out $(cat $WALLET_PATH/$BOARD_MEMBER3.addr)+20000000 \
    --tx-out $(cat $WALLET_PATH/$BOARD_MEMBER4.addr)+20000000 \
    --tx-out $(cat $WALLET_PATH/$BOARD_MEMBER5.addr)+20000000 \
    --tx-out $(cat $WALLET_PATH/$DEBUGGER.addr)+100000000 \
    --tx-out $(cat $WALLET_PATH/$SENDER.addr)+100000000 \
    --tx-out $(cat $WALLET_PATH/$RECEIVER.addr)+20000000 \
    $(echo $FUND_ADMINS_TXOUTS) \
    --out-file $TX_PATH/tx.raw
)

echo $fund_tx_building

cardano-cli conway transaction sign \
    --tx-body-file $TX_PATH/tx.raw \
    --signing-key-file ../cardano-private-testnet-setup/private-testnet/utxo-keys/utxo1.skey \
    --testnet-magic ${TESTNET_MAGIC} \
    --out-file $TX_PATH/tx.signed

cardano-cli conway transaction submit \
    --testnet-magic ${TESTNET_MAGIC} \
    --tx-file $TX_PATH/tx.signed

tx_submitted $TX_PATH/tx.signed $(cat $WALLET_PATH/$BOARD_MEMBER1.addr)

for (( i=1; i<=$AM_ADMINS; i++ ))
do  
    balanceUser $ORACLE_ADMIN$i
done

##### create cat
printfGreen "create cat ref..."
./02-create-cat.sh $DEBUGGER $REF_SCR_WALLET

##### mint board member auth tokens
printfGreen "create bmats..."
./03-create-bmat.sh $BOARD_MEMBER1 & 
./03-create-bmat.sh $BOARD_MEMBER2 & 
./03-create-bmat.sh $BOARD_MEMBER3 & 
./03-create-bmat.sh $BOARD_MEMBER4 & 
./03-create-bmat.sh $BOARD_MEMBER5 

wait

##### create controller utxo
printfGreen "create controller..."
./04-create-controller.sh $BOARD_MEMBER1 $REF_SCR_WALLET

##### create oracle ref utxo
printfGreen "create oracle ref scripts..."
for (( i=1; i<=$AM_ADMINS; i++ ))
do  
    ./05addr-create-oracle-ref.sh $BOARD_MEMBER1 $REF_SCR_WALLET $i $DEBUGGER $ORACLE_ADMIN$i $TREASURY
done

printfGreen "mint tdat..."

for (( i=1; i<=$AM_ADMINS; i++ ))
do 
    ./06addr-mint-tdat.sh $ORACLE_ADMIN$i $i $RECEIVER 7
done

balanceValidator threatDatabase1
printfGreen "Now we minted our first threat data utxo, verified by it's currency symbol $(cat $Policy_Path/tdat1.cs) and locked it \n
at the threat database validator."
cat $DATUM_PATH/threat-database-dat1.json | jq
printfGreen "In the datum we can find the threat data. First we have a tag to differenciate between data, second the threat score, \n
third some sample threat data (a list of addresses) and fourth the time it was last updated (POSIX time stamp)."
printfGreen "Currently we have a threat score of 5 which is relatively high."

#pressContinue
printfGreen "lock escrow"
./08-lock-escrow.sh $SENDER $RECEIVER 50000000 1
cardano-cli query utxo --testnet-magic $TESTNET_MAGIC --address $(cat $Validator_Path/escrow.addr)
printfGreen "A sender has locked 50 Ada for a receiver at an escrow contract which will first read the threat data from the threat \n
database validator." 

printfGreen "unlock escrow"
./09-unlock-escrow.sh $RECEIVER 1
cardano-cli query utxo --testnet-magic $TESTNET_MAGIC --address $(cat $WALLET_PATH/$RECEIVER.addr)
printfGreen "If the receiver now wants to unlock his 50 Ada, he cannot do it because the threat is score is above 5 and his \n
address is on the malicious threat address list."

printfGreen "update threat score..."
for (( i=1; i<=$AM_ADMINS; i++ ))
do 
    ./07a-update-threat-score.sh $ORACLE_ADMIN$i $i 4
done

balanceValidator threatDatabase1
cat $DATUM_PATH/threat-database-dat1.json | jq
printfGreen "Now we have updated the threat score below 5 to 4, the receiver should be able to unlock his Ada."

printfGreen "unlock escrow"
./09-unlock-escrow.sh $RECEIVER 1
cardano-cli query utxo --testnet-magic $TESTNET_MAGIC --address $(cat $Validator_Path/escrow.addr)
printfGreen "The escrow validator is now empty."
cardano-cli query utxo --testnet-magic $TESTNET_MAGIC --address $(cat $WALLET_PATH/$RECEIVER.addr)
printfGreen "We can see that the receiver was able to unlock his ada."
