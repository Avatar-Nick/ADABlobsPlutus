set -eux

bodyFile=temp/consolidate-tx-body.01
signingKey=~/$BLOCKCHAIN_PREFIX/buyer.skey
outFile=temp/consolidate-tx.01
buyerAddr=$(cat ~/$BLOCKCHAIN_PREFIX/buyer.addr)
burnAddr=$(cat ~/$BLOCKCHAIN_PREFIX/burn.addr)

cardano-cli transaction build \
  --alonzo-era \
  $BLOCKCHAIN \
  --tx-in 0cab3709cf67288f2a324bd225092bceb231106f1d614521ead1ea878f8c6c11#2 \
  --tx-out "$burnAddr + 2829131 lovelace + 1 0af24bf113b80129257898dd7dda43fd1ee950688a5b1273dc8f3bfc.6578616d706c654e6674 + 1 79f2c52b856e89098130f14e96f3768257dbfa71756586b60424ca1d.123456 + 11 d6cfdbedd242056674c0e51ead01785497e3a48afbbb146dc72ee1e2.123456" \
  --change-address $burnAddr \
  --protocol-params-file scripts/$BLOCKCHAIN_PREFIX/protocol-parameters.json \
  --out-file $bodyFile

echo "saved transaction to $bodyFile"

cardano-cli transaction sign \
   --tx-body-file $bodyFile \
   --signing-key-file $signingKey \
   $BLOCKCHAIN \
   --out-file $outFile

echo "signed transaction and saved as $outFile"

cardano-cli transaction submit \
 $BLOCKCHAIN \
 --tx-file $outFile

echo "submitted transaction"
