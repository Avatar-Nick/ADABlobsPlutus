set -eux
thisDir=$(dirname "$0")
baseDir=$thisDir/../
tempDir=$baseDir/../
bn=$(basename $0)

$baseDir/wait/until-next-block.sh

# echo Mint
$baseDir/minting/mint-0-policy.sh
sleep 2
$baseDir/wait/until-next-block.sh

$baseDir/accounts/log-all-accounts.sh $bn 0

echo Start Auction
$baseDir/happy-path/lock-tx.sh 400000 0

startTime=$(date +%s)

sleep 2
$baseDir/wait/until-next-block.sh

$baseDir/accounts/log-all-accounts.sh $bn 1
$baseDir/accounts/diff-accounts.sh $bn 0 1

echo Early Close Fails
detected=false

"$baseDir/failure-cases/close-too-early-tx.sh" || {
    detected=true
}

if [ $detected == false ]; then
  exit 1
fi

$baseDir/accounts/log-all-accounts.sh $bn 2
$baseDir/accounts/diff-accounts.sh $bn 1 2

echo Reserve Not Met Fails
detected=false

"$baseDir/failure-cases/reserve-not-met-tx.sh" || {
    detected=true
}

if [ $detected == false ]; then
  exit 1
fi

$baseDir/accounts/log-all-accounts.sh $bn 3
$baseDir/accounts/diff-accounts.sh $bn 2 3

echo First Bid
$baseDir/happy-path/bid-1-tx.sh
sleep 2
$baseDir/wait/until-next-block.sh

$baseDir/accounts/log-all-accounts.sh $bn 4
$baseDir/accounts/diff-accounts.sh $bn 3 4

echo Second Bid
$baseDir/happy-path/bid-2-tx.sh
sleep 2
$baseDir/wait/until-next-block.sh

$baseDir/accounts/log-all-accounts.sh $bn 5
$baseDir/accounts/diff-accounts.sh $bn 4 5

echo Failed Bid
detected=false

"$baseDir/failure-cases/bid-not-high-enough.sh" || {
    detected=true
}

if [ $detected == false ]; then
  exit 1
fi

$baseDir/accounts/log-all-accounts.sh $bn 6
$baseDir/accounts/diff-accounts.sh $bn 5 6

endTime=$(date +%s)
elapsedTime=$(($endTime-$startTime))
sleepTime=$((555 - $elapsedTime))
sleep $sleepTime

echo Close with Wrong Payout Fails
detected=false

"$baseDir/failure-cases/close-wrong-payout-tx.sh" || {
    detected=true
}

if [ $detected == false ]; then
  exit 1
fi

$baseDir/accounts/log-all-accounts.sh $bn 7
$baseDir/accounts/diff-accounts.sh $bn 6 7

echo Close
$baseDir/happy-path/close-tx.sh
sleep 2
$baseDir/wait/until-next-block.sh

$baseDir/accounts/log-all-accounts.sh $bn 8
$baseDir/accounts/diff-accounts.sh $bn 7 8

echo Bid on Expired Auction Fails
detected=false
"$baseDir/failure-cases/bid-on-expired-auction.sh" || {
    detected=true
}

if [ $detected == false ]; then
  exit 1
fi

$baseDir/accounts/log-all-accounts.sh $bn 9
$baseDir/accounts/diff-accounts.sh $bn 8 9
