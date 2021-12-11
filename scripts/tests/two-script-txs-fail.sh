set -eux
thisDir=$(dirname "$0")
baseDir=$thisDir/../
bn=$(basename $0)

$baseDir/wait/until-next-block.sh

export DATUM_PREFIX=0

echo Mint 1
$baseDir/minting/mint-0-policy.sh
$baseDir/wait/until-next-block.sh

$baseDir/accounts/log-all-accounts.sh $bn 0

echo Start Auction 1
$baseDir/happy-path/lock-tx.sh 500000
sleep 2
$baseDir/wait/until-next-block.sh

$baseDir/accounts/log-all-accounts.sh $bn 1
$baseDir/accounts/diff-accounts.sh $bn 0 1

echo First Bid 1
$baseDir/happy-path/bid-1-tx.sh
sleep 2
$baseDir/wait/until-next-block.sh

$baseDir/accounts/log-all-accounts.sh $bn 2
$baseDir/accounts/diff-accounts.sh $bn 1 2

echo Mint 2
$baseDir/minting/mint-0-policy.sh
sleep 2
$baseDir/wait/until-next-block.sh

$baseDir/accounts/log-all-accounts.sh $bn 3
$baseDir/accounts/diff-accounts.sh $bn 2 3


export DATUM_PREFIX=1

echo Start Auction 2
$baseDir/happy-path/lock-tx.sh 550000 1
startTime1=$(date +%s)
sleep 2
$baseDir/wait/until-next-block.sh

$baseDir/accounts/log-all-accounts.sh $bn 4
$baseDir/accounts/diff-accounts.sh $bn 3 4

echo First Bid 2
$baseDir/happy-path/seller-bid-1-tx.sh
sleep 2
$baseDir/wait/until-next-block.sh

$baseDir/accounts/log-all-accounts.sh $bn 5
$baseDir/accounts/diff-accounts.sh $bn 4 5

export DATUM_PREFIX=0

echo Second Bid 1
$baseDir/happy-path/bid-2-tx.sh
sleep 2
$baseDir/wait/until-next-block.sh

$baseDir/accounts/log-all-accounts.sh $bn 6
$baseDir/accounts/diff-accounts.sh $bn 5 6

endTime=$(date +%s)
elapsedTime=$(($endTime-$startTime1))
sleepTime=$((605 - $elapsedTime))
sleep $sleepTime

"$baseDir/failure-cases/outbid-close-tx.sh" || {
    detected=true
}

if [ $detected == false ]; then
  exit 1
fi

$baseDir/accounts/log-all-accounts.sh $bn 7
$baseDir/accounts/diff-accounts.sh $bn 6 7

echo Success!
