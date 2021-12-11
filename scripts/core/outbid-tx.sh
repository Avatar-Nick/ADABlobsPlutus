set -eu

thisDir=$(dirname "$0")
baseDir=$thisDir/..

buyerAddr=$1
signingKey=$2
value=$3
oldDatumFile=$4
oldDatumHash=$5
newDatumHash=$6
newDatumFile=$7
bidAmount=$8
redeemerFile=$9
oldBidder="${10}"
oldAmount="${11}"

nftValidatorFile=$baseDir/auction.plutus
scriptHash=$(cat $baseDir/$BLOCKCHAIN_PREFIX/auction.addr)

$baseDir/hash-plutus.sh
bodyFile=temp/bid-tx-body.01
outFile=temp/bid-tx.01

utxoScript=$(scripts/query/sc.sh | grep $oldDatumHash | grep $value | head -n 1 | cardano-cli-balance-fixer parse-as-utxo)
currentSlot=$(cardano-cli query tip $BLOCKCHAIN | jq .slot)
startSlot=$currentSlot
nextTenSlots=$(($currentSlot+150))
changeOutput=$(cardano-cli-balance-fixer change --address $buyerAddr $BLOCKCHAIN)

extraOutput=""
if [ "$changeOutput" != "" ];then
  extraOutput="+ $changeOutput"
fi

cardano-cli transaction build \
    --alonzo-era \
    $BLOCKCHAIN \
    $(cardano-cli-balance-fixer input --address $buyerAddr $BLOCKCHAIN ) \
    --tx-in $utxoScript \
    --tx-in-script-file $nftValidatorFile \
    --tx-in-datum-file $oldDatumFile \
    --tx-in-redeemer-file $redeemerFile \
    --required-signer $signingKey \
    --tx-in-collateral $(cardano-cli-balance-fixer collateral --address $buyerAddr $BLOCKCHAIN) \
    --tx-out "$scriptHash + $bidAmount lovelace + 2000000 lovelace + 1 $value" \
    --tx-out-datum-hash $newDatumHash \
    --tx-out-datum-embed-file $newDatumFile \
    --tx-out "$buyerAddr + 3000000 lovelace $extraOutput" \
    --tx-out "$oldBidder + $oldAmount lovelace" \
    --change-address $buyerAddr \
    --protocol-params-file scripts/$BLOCKCHAIN_PREFIX/protocol-parameters.json \
    --invalid-before $startSlot\
    --invalid-hereafter $nextTenSlots \
    --out-file $bodyFile

echo "saved transaction to $bodyFile"

cardano-cli transaction sign \
   --tx-body-file $bodyFile \
   --signing-key-file $signingKey \
   $BLOCKCHAIN \
   --out-file $outFile

echo "signed transaction and saved as $outFile"

cardano-cli transaction submit \
  $BLOCKCHAIN \
  --tx-file $outFile

echo "submitted transaction"

echo
