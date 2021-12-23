set -eux

mkdir -p temp/$BLOCKCHAIN_PREFIX/pkhs

cardano-cli address key-hash --payment-verification-key-file ~/$BLOCKCHAIN_PREFIX/$1.vkey \
 > temp/$BLOCKCHAIN_PREFIX/pkhs/$1-pkh.txt
