set -eu
thisDir=$(dirname "$0")
cardano-cli query utxo --address $(cat $thisDir/../$BLOCKCHAIN_PREFIX/auction.addr) $BLOCKCHAIN
