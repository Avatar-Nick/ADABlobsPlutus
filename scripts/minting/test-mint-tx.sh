set -eux

mkdir -p temp

utxo=$1
policyScript=$2
policyId=$3
tokenName=$4
mintCount=$5
address=$(cat ~/$BLOCKCHAIN_PREFIX/seller.addr)
holderAddress=$(cat ~/$BLOCKCHAIN_PREFIX/seller.addr)

cardano-cli transaction build \
  --alonzo-era \
  $BLOCKCHAIN \
  --tx-in $utxo \
  --tx-in-collateral $utxo \
  --tx-out "$holderAddress + 1758582 lovelace + $mintCount $policyId.$tokenName" \
  --mint="$mintCount $policyId.$tokenName" \
  --minting-script-file $policyScript \
  --mint-redeemer-value [] \
  --change-address $address \
  --protocol-params-file scripts/$BLOCKCHAIN_PREFIX/protocol-parameters.json \
  --out-file temp/mint_tx.body

cardano-cli transaction sign  \
  --signing-key-file ~/$BLOCKCHAIN_PREFIX/seller.skey  \
  $BLOCKCHAIN \
  --tx-body-file temp/mint_tx.body \
  --out-file temp/mint_tx.signed

cardano-cli transaction submit --tx-file temp/mint_tx.signed $BLOCKCHAIN
