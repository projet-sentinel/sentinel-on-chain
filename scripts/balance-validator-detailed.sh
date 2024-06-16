source env.sh

cardano-cli query utxo --testnet-magic ${TESTNET_MAGIC} --address $(cat $Validator_Path/$1.addr) --out-file validator.utxos

detailedUtxos=$(cat validator.utxos)
echo $detailedUtxos | jq
rm validator.utxos