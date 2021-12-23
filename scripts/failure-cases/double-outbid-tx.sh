set -eux
thisDir=$(dirname "$0")
baseDir=$thisDir/../
tempDir=$baseDir/../temp

$baseDir/hash-datums.sh
$baseDir/hash-plutus.sh

buyerAddr=$(cat ~/$BLOCKCHAIN_PREFIX/attacker.addr)
signingKey=~/$BLOCKCHAIN_PREFIX/attacker.skey
value=d6cfdbedd242056674c0e51ead01785497e3a48afbbb146dc72ee1e2.123456
oldDatumFile0=$tempDir/$BLOCKCHAIN_PREFIX/datums/bid-1.json
oldDatumHash0=$(cat $tempDir/$BLOCKCHAIN_PREFIX/datums/0/bid-1-hash.txt)
newDatumHash0=$(cat $tempDir/$BLOCKCHAIN_PREFIX/datums/0/bid-2-hash.txt)
oldDatumFil1=$tempDir/$BLOCKCHAIN_PREFIX/datums/1/bid-1.json
oldDatumHash1=$(cat $tempDir/$BLOCKCHAIN_PREFIX/datums/1/bid-1-hash.txt)
newDatumHash1=$(cat $tempDir/$BLOCKCHAIN_PREFIX/datums/1/bid-2-hash.txt)
newDatumFile=$tempDir/$BLOCKCHAIN_PREFIX/datums/0/bid-2.json
bidAmount=30000000
redeemerFile=$tempDir/$BLOCKCHAIN_PREFIX/redeemers/bid-2.json
oldBidder=$(cat ~/$BLOCKCHAIN_PREFIX/buyer.addr)
oldAmount=10000000

nftValidatorFile=$baseDir/auction.plutus
scriptHash=$(cat $baseDir/$BLOCKCHAIN_PREFIX/auction.addr)

bodyFile=temp/bid-tx-body.01
outFile=temp/bid-tx.01

utxoScript0=$(scripts/query/sc.sh | grep $oldDatumHash0 | grep $value | head -n 1 | cardano-cli-balance-fixer parse-as-utxo)
utxoScript1=$(scripts/query/sc.sh | grep $oldDatumHash1 | grep $value | tail -n 1 | cardano-cli-balance-fixer parse-as-utxo)
currentSlot=$(cardano-cli query tip $BLOCKCHAIN | jq .slot)
startSlot=$(($currentSlot-1))
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
    --tx-in $utxoScript0 \
    --tx-in-script-file $nftValidatorFile \
    --tx-in-datum-file $oldDatumFile0 \
    --tx-in-redeemer-file $redeemerFile \
    --tx-in $utxoScript1 \
    --tx-in-script-file $nftValidatorFile \
    --tx-in-datum-file $oldDatumFile1 \
    --tx-in-redeemer-file $redeemerFile \
    --required-signer $signingKey \
    --tx-in-collateral $(cardano-cli-balance-fixer collateral --address $buyerAddr $BLOCKCHAIN) \
    --tx-out "$scriptHash + $bidAmount lovelace + 1 $value" \
    --tx-out-datum-hash $newDatumHash \
    --tx-out-datum-embed-file $newDatumFile \
    --tx-out "$buyerAddr + 3000000 lovelace $extraOutput" \
    --tx-out "$buyerAddr + $oldAmount lovelace" \
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
