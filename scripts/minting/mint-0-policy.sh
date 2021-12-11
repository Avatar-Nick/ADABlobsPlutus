./scripts/minting/test-mint-tx.sh \
  $(cardano-cli-balance-fixer collateral --address $(cat ~/$BLOCKCHAIN_PREFIX/seller.addr) $BLOCKCHAIN) \
   scripts/test-policies/test-policy-0.plutus \
   $(cat scripts/test-policies/test-policy-0-id.txt) 123456 1
