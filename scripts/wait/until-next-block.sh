initialBlock=$(cardano-cli query tip $BLOCKCHAIN 2>/dev/null | jq .slot )
seconds=0
echo "initial block $initialBlock"
while true
do
  nextSlot=$(cardano-cli query tip $BLOCKCHAIN 2>/dev/null | jq .slot  )
  echo "current block $nextSlot"
  if [ $initialBlock == $nextSlot ]
    then
      echo "waiting for next block for $seconds seconds"
      sleep 1
      seconds=$(($seconds+1))
    else
      echo "Found next block!"
      break
    fi
done

# Do the same thing twice on mainnet
if [ $BLOCKCHAIN_PREFIX == "mainnet" ]; then
initialBlock=$(cardano-cli query tip $BLOCKCHAIN 2>/dev/null | jq .slot )
seconds=0
echo "initial block $initialBlock"
while true
do
  nextSlot=$(cardano-cli query tip $BLOCKCHAIN 2>/dev/null | jq .slot  )
  echo "current block $nextSlot"
  if [ $initialBlock == $nextSlot ]
    then
      echo "waiting for next block for $seconds seconds"
      sleep 1
      seconds=$(($seconds+1))
    else
      echo "Found next block!"
      exit
    fi
done
fi
