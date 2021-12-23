set -eux

thisDir=$(dirname "$0")
baseDir=$thisDir/..
tempDir=$baseDir/../temp

attackerAddr=$(cat ~/$BLOCKCHAIN_PREFIX/attacker.addr)
signingKey=~/$BLOCKCHAIN_PREFIX/attacker.skey
value="d6cfdbedd242056674c0e51ead01785497e3a48afbbb146dc72ee1e2.123456"
datumFile0=$tempDir/$BLOCKCHAIN_PREFIX/datums/0/bid-2.json
datumHash0=$(cat $baseDir/$BLOCKCHAIN_PREFIX/datums/0/bid-2-hash.txt)
datumFile1=$tempDir/$BLOCKCHAIN_PREFIX/datums/1/bid-2.json
datumHash1=$(cat $tempDir/$BLOCKCHAIN_PREFIX/datums/1/bid-2-hash.txt)
winningBuyer=$(cat ~/$BLOCKCHAIN_PREFIX/buyer1.addr)
sellerAmount=27000000
sellerAddr=$(cat ~/$BLOCKCHAIN_PREFIX/seller.addr)
royaltyAmount=1500000
royaltyAddr=$(cat ~/$BLOCKCHAIN_PREFIX/royalities.addr)
marketplaceAmount=1500000
marketplaceAddr=$(cat ~/$BLOCKCHAIN_PREFIX/marketplace.addr)


nftValidatorFile=$baseDir/auction.plutus
scriptHash=$(cat $baseDir/$BLOCKCHAIN_PREFIX/auction.addr)

$baseDir/hash-plutus.sh
bodyFile=temp/close-tx-body.01
outFile=temp/close-tx.01
redeemerFile="$tempDir/$BLOCKCHAIN_PREFIX/redeemers/close.json"
utxoScript0=$(scripts/query/sc.sh | grep $datumHash0 | grep $value | head -n 1 | cardano-cli-balance-fixer parse-as-utxo)
utxoScript1=$(scripts/query/sc.sh | grep $datumHash1 | grep $value | tail -n 1 | cardano-cli-balance-fixer parse-as-utxo)
output1="1724100 lovelace + 1 $value"
currentSlot=$(cardano-cli query tip $BLOCKCHAIN | jq .slot)
startSlot=$(($currentSlot-1))
nextTenSlots=$(($currentSlot+150))

cardano-cli transaction build \
    --alonzo-era \
    $BLOCKCHAIN \
    $(cardano-cli-balance-fixer input --address $sellerAddr $BLOCKCHAIN ) \
    --tx-in $utxoScript0 \
    --tx-in-script-file $nftValidatorFile \
    --tx-in-datum-file $datumFile0 \
    --tx-in-redeemer-file $redeemerFile \
    --tx-in $utxoScript1 \
    --tx-in-script-file $nftValidatorFile \
    --tx-in-datum-file $datumFile1 \
    --tx-in-redeemer-file $redeemerFile \
    --required-signer $signingKey \
    --tx-in-collateral $(cardano-cli-balance-fixer collateral --address $sellerAddr $BLOCKCHAIN) \
    --tx-out "$winningBuyer + $output1" \
    --tx-out "$sellerAddr + $sellerAmount lovelace + $(cardano-cli-balance-fixer change --address $sellerAddr $BLOCKCHAIN)" \
    --tx-out "$royaltyAddr + $royaltyAmount lovelace "  \
    --tx-out "$marketplaceAddr + $marketplaceAmount lovelace "  \
    --tx-out "$attackerAddr + $sellerAmount lovelace" \
    --tx-out "$attackerAddr + $royaltyAmount lovelace" \
    --tx-out "$attackerAddr + $marketplaceAmount lovelace" \
    --change-address $sellerAddr \
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
