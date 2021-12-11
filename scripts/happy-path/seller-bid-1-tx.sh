set -eu
thisDir=$(dirname "$0")
baseDir=$thisDir/../

DATUM_PREFIX=${DATUM_PREFIX:-0}

$baseDir/core/bid-tx.sh \
  $(cat ~/$BLOCKCHAIN_PREFIX/seller.addr) \
  ~/$BLOCKCHAIN_PREFIX/seller.skey \
  d6cfdbedd242056674c0e51ead01785497e3a48afbbb146dc72ee1e2.123456 \
  $baseDir/$BLOCKCHAIN_PREFIX/datums/$DATUM_PREFIX/start.json \
  $(cat $baseDir/$BLOCKCHAIN_PREFIX/datums/$DATUM_PREFIX/start-hash.txt) \
  $(cat $baseDir/$BLOCKCHAIN_PREFIX/datums/$DATUM_PREFIX/seller-bid-1-hash.txt) \
  $baseDir/$BLOCKCHAIN_PREFIX/datums/$DATUM_PREFIX/seller-bid-1.json \
  10000000 \
  $baseDir/$BLOCKCHAIN_PREFIX/redeemers/seller-bid-1.json
