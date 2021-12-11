set -eux
thisDir=$(dirname "$0")

cardano-cli address build \
  --payment-script-file $thisDir/auction.plutus \
  $BLOCKCHAIN \
  --out-file $thisDir/$BLOCKCHAIN_PREFIX/auction.addr
