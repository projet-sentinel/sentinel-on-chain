#!/bin/bash
# sampel input: ./00-setup.sh

chmod +x 01-start-private-testnet.sh
chmod +x 02-create-cat.sh
chmod +x 03-create-bmat.sh
chmod +x 04-create-controller.sh
chmod +x 05addr-create-oracle-ref.sh
chmod +x 05sh-create-oracle-ref.sh
chmod +x 06addr-mint-tdat.sh
chmod +x 06sh-mint-tdat.sh
chmod +x 07a-update-threat-score.sh
chmod +x 07b-remove-threat-score.sh
chmod +x 08-lock-escrow.sh
chmod +x 09-unlock-escrow.sh

chmod +x balance-wallet.sh
chmod +x balance-validator.sh
chmod +x create-user-stake.sh
chmod +x fund-user.sh
chmod +x write-env.sh

cd ..
cd run-examples

chmod +x multiple-dbs.sh
chmod +x run-full-trace.sh
chmod +x threat-data-reader.sh
chmod +x wrong-admin.sh
chmod +x wrong-treasury.sh

git clone https://gitlab.com/gimbalabs/andamio/cardano-private-testnet-setup

cd cardano-private-testnet-setup
git checkout cardano-node8.9.0