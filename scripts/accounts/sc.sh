set -eu

thisDir=$(dirname "$0")
baseDir=$thisDir/..

cardano-cli-balance-fixer balance --address $(cat $baseDir/$BLOCKCHAIN_PREFIX/auction.addr) $BLOCKCHAIN
