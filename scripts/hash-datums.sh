set -eux
thisDir=$(dirname "$0")
tempDir=$thisDir/../temp

find $tempDir/$BLOCKCHAIN_PREFIX/datums -name "*.json" -exec sh -c 'cardano-cli transaction hash-script-data --script-data-file "$1" > "${1%.*}-hash.txt"' sh {} \;
