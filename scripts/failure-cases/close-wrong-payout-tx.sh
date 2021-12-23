set -eux
thisDir=$(dirname "$0")
baseDir=$thisDir/../
tempDir=$baseDir/../temp

DATUM_PREFIX=${DATUM_PREFIX:-0}

$baseDir/core/close-successfully-tx.sh \
  $(cat ~/$BLOCKCHAIN_PREFIX/seller.addr) \
  ~/$BLOCKCHAIN_PREFIX/marketplace.skey \
  "d6cfdbedd242056674c0e51ead01785497e3a48afbbb146dc72ee1e2.123456" \
  $baseDir/$BLOCKCHAIN_PREFIX/datums/$DATUM_PREFIX/bid-2.json \
  $(cat $baseDir/$BLOCKCHAIN_PREFIX/datums/$DATUM_PREFIX/bid-2-hash.txt) \
  $(cat ~/$BLOCKCHAIN_PREFIX/buyer1.addr) \
  26000000 \
  $(cat ~/$BLOCKCHAIN_PREFIX/seller.addr) \
  1600000 \
  $(cat ~/$BLOCKCHAIN_PREFIX/royalities.addr) \
  1500000 \
  $(cat ~/$BLOCKCHAIN_PREFIX/marketplace.addr)
