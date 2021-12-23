set -eux
thisDir=$(dirname "$0")
baseDir=$thisDir/../
tempDir=$baseDir/../temp

$baseDir/hash-plutus.sh
$baseDir/hash-datums.sh

nftValidatorFile=$baseDir/auction.plutus
scriptHash=$(cat $baseDir/$BLOCKCHAIN_PREFIX/auction.addr)
bodyFile=temp/close-tx-body.01
outFile=temp/close-tx.01


sellerAddr=$(cat ~/$BLOCKCHAIN_PREFIX/seller.addr)
value="d6cfdbedd242056674c0e51ead01785497e3a48afbbb146dc72ee1e2.123456"
closeDatumFile=$tempDir/$BLOCKCHAIN_PREFIX/datums/0/bid-2.json
closeDatumHash=$(cat $tempDir/$BLOCKCHAIN_PREFIX/datums/0/bid-2-hash.txt)
winningBuyer=$(cat ~/$BLOCKCHAIN_PREFIX/buyer1.addr)
sellerAmount=27000000
royaltyAmount=1500000
royaltyAddr=$(cat ~/$BLOCKCHAIN_PREFIX/royalities.addr)
marketplaceAmount=1500000
marketplaceAddr=$(cat ~/$BLOCKCHAIN_PREFIX/marketplace.addr)

closeRedeemerFile="$tempDir/$BLOCKCHAIN_PREFIX/redeemers/close.json"


output1="1724100 lovelace + 1 $value"

buyerAddr=$(cat ~/$BLOCKCHAIN_PREFIX/attacker.addr)
signingKey=~/$BLOCKCHAIN_PREFIX/attacker.skey
value=d6cfdbedd242056674c0e51ead01785497e3a48afbbb146dc72ee1e2.123456
oldDatumFile=$tempDir/$BLOCKCHAIN_PREFIX/datums/1/seller-bid-1.json
oldDatumHash=$(cat $baseDir/$BLOCKCHAIN_PREFIX/datums/1/seller-bid-1-hash.txt)
newDatumHash=$(cat $tempDir/$BLOCKCHAIN_PREFIX/datums/1/bid-2-hash.txt)
newDatumFile=$tempDir/$BLOCKCHAIN_PREFIX/datums/1/bid-2.json
bidAmount=30000000
redeemerFile=$baseDir/$BLOCKCHAIN_PREFIX/redeemers/bid-2.json
oldBidder=$(cat ~/$BLOCKCHAIN_PREFIX/buyer.addr)
oldAmount=10000000



closeScriptUtxo=$(scripts/query/sc.sh | grep $closeDatumHash | grep $value | head -n 1 | cardano-cli-balance-fixer parse-as-utxo)
bidScriptUtxo=$(scripts/query/sc.sh | grep $oldDatumHash | grep $value | head -n 1 | cardano-cli-balance-fixer parse-as-utxo)

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
    --tx-in $bidScriptUtxo \
    --tx-in-script-file $nftValidatorFile \
    --tx-in-datum-file $oldDatumFile \
    --tx-in-redeemer-file $redeemerFile \
    --tx-in $closeScriptUtxo \
    --tx-in-script-file $nftValidatorFile \
    --tx-in-datum-file $closeDatumFile \
    --tx-in-redeemer-file $closeRedeemerFile \
    --required-signer $signingKey \
    --tx-in-collateral $(cardano-cli-balance-fixer collateral --address $sellerAddr $BLOCKCHAIN) \
    --tx-out "$winningBuyer + $output1" \
    --tx-out "$sellerAddr + $sellerAmount lovelace + $(cardano-cli-balance-fixer change --address $sellerAddr $BLOCKCHAIN)" \
    --tx-out "$royaltyAddr + $royaltyAmount lovelace "  \
    --tx-out "$marketplaceAddr + $marketplaceAmount lovelace "  \
    --tx-out "$scriptHash + $bidAmount lovelace + 1 $value" \
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
