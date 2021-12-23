set -eux

if [ ! -f ~/$BLOCKCHAIN_PREFIX/$1.vkey  ]; then
cardano-cli address key-gen --verification-key-file ~/$BLOCKCHAIN_PREFIX/$1.vkey --signing-key-file ~/$BLOCKCHAIN_PREFIX/$1.skey
cardano-cli address build $BLOCKCHAIN --payment-verification-key-file ~/$BLOCKCHAIN_PREFIX/$1.vkey --out-file ~/$BLOCKCHAIN_PREFIX/$1.addr
fi
