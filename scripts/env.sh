#!/bin/bash

# Path

find_and_set_repo_home() {
    local target_folder="sentinel-on-chain-private"

    local repo_path
    repo_path=$(find /home -type d -name "$target_folder" -not -path '*/\.*' -print -quit 2>/dev/null)

    if [ -n "$repo_path" ]; then
        echo "$repo_path"
    else
        echo "$target_folder not found."
        exit
    fi
}

export REPO_HOME=$(find_and_set_repo_home) #path to this repository
export CARDANO_NODE_SOCKET_PATH=$(cat config.json | jq -r '.CARDANO_NODE_SOCKET_PATH')
export NETWORK_DIR_PATH=$(cat config.json | jq -r '.NETWORK_DIR_PATH')
export TESTNET_MAGIC=$(cat config.json | jq -r '.TESTNET_MAGIC')

##
export TX_PATH="$NETWORK_DIR_PATH/tx"
export WALLET_PATH="$NETWORK_DIR_PATH/wallets"

##### Datums
export DATUM_PATH="$NETWORK_DIR_PATH/datums"

##### Redeemers
export REDEEMER_PATH="$NETWORK_DIR_PATH/redeemers"

##### Validators
export Validator_Path="$NETWORK_DIR_PATH/validators"

export CONTROLLER="$Validator_Path/controller.plutus"
export CONTROLLER_ADDR="$Validator_Path/controller.addr"
export CONTROLLER_VH="$Validator_Path/controller.vh"

##### Stake Validators
export StakeVal_Path="$NETWORK_DIR_PATH/stake-validators"

##### Policies
export Policy_Path="$NETWORK_DIR_PATH/policies"

export CAT_Policy="$Policy_Path/cat.policy"
export CAT_CS="$Policy_Path/cat.cs"

export BMAT_Policy="$Policy_Path/bmat.plutus"
export BMAT_CS="$Policy_Path/bmat.cs"

wait_for_node(){

    while [[ ! -S "$CARDANO_NODE_SOCKET_PATH" ]]; do
        printfCyan "nodes not ready..."
        sleep 5
    done
}

run_node(){
    open_terminal_and_run_commands ./01-start-private-testnet.sh || exit 1
    sleep 5
}

wait_tx_submitted(){
    old_blocks=$(cardano-cli query tip --testnet-magic ${TESTNET_MAGIC} | jq '.block')
    new_blocks=$(cardano-cli query tip --testnet-magic ${TESTNET_MAGIC} | jq '.block')

    while [[ "$old_blocks" == "$new_blocks" ]]
    do
        new_blocks=$(cardano-cli query tip --testnet-magic ${TESTNET_MAGIC} | jq '.block')
    done
}

open_terminal_and_run_commands() {
    local commands_to_run="$@"
    gnome-terminal -- bash -c "$commands_to_run; exec bash"
}

get_address_biggest_lovelace() {
    cardano-cli query utxo --address $1 --testnet-magic ${TESTNET_MAGIC} |
        tail -n +3 |
        awk '{printf "%s#%s %s \n", $1 , $2, $3}' |
        sort -rn -k2 |
        head -n1 |
        awk '{print $1}'
}

get_UTxO_by_token() {
    for i in {1..50}; do
        utxoAttachedHex=$(
            cardano-cli query utxo --address $1 --testnet-magic ${TESTNET_MAGIC} |
                tail -n +3 |
                awk '{printf "%s %s#%s %s\n",$6, $1, $2, $7}' |
                sort -gr |
                awk -v i="$i" 'NR == i {printf("%s", $3)}'
        )
        if [ "$utxoAttachedHex" == "$2" ]; then
            cardano-cli query utxo --address $1 --testnet-magic ${TESTNET_MAGIC} |
                tail -n +3 |
                awk '{printf "%s %s#%s\n",$6, $1, $2}' |
                sort -gr |
                awk -v i="$i" 'NR == i {printf("%s", $2)}'
            return
        fi
    done
}

get_UTxO_lovelace_amount() {
    local ADDRESS="$1"
    local UTxO="$2"

    local utxo_info
    utxo_info=$(cardano-cli query utxo --address "$ADDRESS" --testnet-magic "${TESTNET_MAGIC}" | tail -n +3)

    local utxo_entries
    IFS=$'\n' read -r -d '' -a utxo_entries <<<"$utxo_info"

    for i in {1..50}; do
        for entry in "${utxo_entries[@]}"; do
            entry_parts=($entry)
            utxo_hash=${entry_parts[0]}
            utxo_id=${entry_parts[1]}

            if [[ "$utxo_hash#$utxo_id" == "$UTxO" ]]; then
                echo "${entry_parts[2]}"
                return 0
            fi
        done
    done

    return 1
}


get_UTxO_lovelace_amount() {
    local ADDRESS="$1"
    local UTxO="$2"

    local utxo_info
    utxo_info=$(cardano-cli query utxo --address "$ADDRESS" --testnet-magic ${TESTNET_MAGIC} | tail -n +3)

    local utxo_entries
    IFS=$'\n' read -r -d '' -a utxo_entries <<<"$utxo_info"

    for i in {1..10}; do
        for entry in "${utxo_entries[@]}"; do
            entry_parts=($entry)
            utxo_hash=${entry_parts[0]}
            utxo_id=${entry_parts[1]}

            if [[ "$utxo_hash#$utxo_id" == "$UTxO" ]]; then
                echo "${entry_parts[2]}"
                return 0
            fi
        done
    done

    return 1
}

get_current_slot_number() {
    cardano-cli query tip --testnet-magic ${TESTNET_MAGIC} |
        awk -v i="6" 'NR == i {printf("%s", $2)}' |
        cut -d ',' -f 1
}

get_current_POSIX_time() {
    local TIME=$(($(date +%s) * 1000))
    echo "$TIME"
}

# $1 = tx file path
# $2 = address first output
tx_submitted(){
    tx_Id=$(cardano-cli transaction txid --tx-file $1)
    cardano-cli query utxo --testnet-magic ${TESTNET_MAGIC} --address $2 --out-file tmp.utxos
    presence=$(jq -r ".[\"$tx_id#0\"]" "tmp.utxos")

    start_time_seconds=$(date +%s)

    if [[ "$TESTNET_MAGIC" == "42" ]]; then
        while [ "$presence" == "null" ] && [[ $(( $run_time_seconds - $start_time_seconds )) < 5 ]]
        do
            run_time_seconds=$(date +%s)

            cardano-cli query utxo --testnet-magic ${TESTNET_MAGIC} --address $2 --out-file tmp.utxos
            presence=$(jq -r ".[\"$tx_Id#0\"]" "tmp.utxos")
        done
        sleep 1
    else 

        while [ "$presence" == "null" ] && [[ $(( $run_time_seconds - $start_time_seconds )) < 600 ]]
        do
            run_time_seconds=$(date +%s)

            cardano-cli query utxo --testnet-magic ${TESTNET_MAGIC} --address $2 --out-file tmp.utxos
            presence=$(jq -r ".[\"$tx_Id#0\"]" "tmp.utxos")
            sleep 5
        done
    fi
    rm tmp.utxos
}

# $1 = database addr
# $2 = tag
get_database_utxo(){
    cardano-cli query utxo --testnet-magic ${TESTNET_MAGIC} --address $1 --out-file index_utxos.tmp
    index_utxo=$(cat index_utxos.tmp | jq --argjson i "$2" 'to_entries[] | select(.value.inlineDatum.fields[0].int==$i)')
    rm index_utxos.tmp
    echo $(echo $index_utxo | jq -r '.key')
}