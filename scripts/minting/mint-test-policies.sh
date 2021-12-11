set -eux

./scripts/minting/test-mint-tx.sh $1 scripts/test-policies/test-policy-0.plutus $(cat scripts/test-policies/test-policy-0-id.txt) 123456 1
./scripts/minting/test-mint-tx.sh $2 scripts/test-policies/test-policy-1.plutus $(cat scripts/test-policies/test-policy-1-id.txt) 7890 1
./scripts/minting/test-mint-tx.sh $3 scripts/test-policies/test-policy-2.plutus $(cat scripts/test-policies/test-policy-2-id.txt) 1357 1000
./scripts/minting/test-mint-tx.sh $4 scripts/test-policies/test-policy-3.plutus $(cat scripts/test-policies/test-policy-3-id.txt) 80 1000
