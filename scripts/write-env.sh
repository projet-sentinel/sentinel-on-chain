#!/bin/bash

find_and_set_repo_home() {
    local target_folder="sentinel-on-chain"

    local repo_path
    repo_path=$(find /home -type d -name "$target_folder" -not -path '*/\.*' -print -quit 2>/dev/null)

    if [ -n "$repo_path" ]; then
        echo "$repo_path"
    else
        echo "$target_folder not found."
        exit
    fi
}

REPO_HOME=$(find_and_set_repo_home) #path to this repository
SOCKET_PATH=""
NETWORK_DIR_PATH="" # path to network in use (preprod/private)
TESTNET_MAGIC=0 # magic refeering to network in use (1/42)

if [ -z "$1" ]; then
    NETWORK_DIR_PATH="$REPO_HOME/private"
    SOCKET_PATH="$REPO_HOME/cardano-private-testnet-setup/private-testnet/node-spo1/node.socket" #path to node socket 
    TESTNET_MAGIC=42
else
    NETWORK_DIR_PATH="$REPO_HOME/preprod"
    SOCKET_PATH="$CARDANO_NODE_SOCKET_PATH"
    TESTNET_MAGIC=1
fi

jq -n \
    --arg CARDANO_NODE_SOCKET_PATH $SOCKET_PATH \
    --arg NETWORK_DIR_PATH "$NETWORK_DIR_PATH" \
    --argjson TESTNET_MAGIC "$TESTNET_MAGIC" \
    '{"NETWORK_DIR_PATH": $NETWORK_DIR_PATH, "CARDANO_NODE_SOCKET_PATH": $CARDANO_NODE_SOCKET_PATH, "TESTNET_MAGIC": $TESTNET_MAGIC }' > config.json
