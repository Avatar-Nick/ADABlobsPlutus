set -eux



thisDir=$(dirname "$0")
baseDir=$thisDir/../
tempDir=$baseDir/../temp

mkdir -p $tempDir
$baseDir/hash-plutus.sh
$baseDir/hash-datums.sh

nftValidatorFile=$baseDir/auction.plutus
sellerAddress=$1
signingKey=$2
scriptDatumHash=$3
output=$4
scriptHash=$(cat $baseDir/$BLOCKCHAIN_PREFIX/auction.addr)

bodyFile=$tempDir/sell-tx-body.01
outFile=$tempDir/sell-tx.01
changeOutput=$(cardano-cli-balance-fixer change --address $sellerAddress $BLOCKCHAIN -o "$output")

extraOutput=""
if [ "$changeOutput" != "" ];then
  extraOutput="+ $changeOutput"
fi

cardano-cli transaction build \
    --alonzo-era \
    $BLOCKCHAIN \
    $(cardano-cli-balance-fixer input --address $sellerAddress $BLOCKCHAIN) \
    --tx-out "$scriptHash + $output" \
    --tx-out-datum-hash $scriptDatumHash \
    --tx-out "$sellerAddress + 1744798 lovelace $extraOutput" \
    --change-address $sellerAddress \
    --protocol-params-file scripts/$BLOCKCHAIN_PREFIX/protocol-parameters.json \
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
