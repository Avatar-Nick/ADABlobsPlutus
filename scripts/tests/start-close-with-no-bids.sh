set -eux
thisDir=$(dirname "$0")
baseDir=$thisDir/../
bn=$(basename $0)

$baseDir/wait/until-next-block.sh

echo Mint
$baseDir/minting/mint-0-policy.sh
sleep 2
$baseDir/wait/until-next-block.sh

$baseDir/accounts/log-all-accounts.sh $bn 0

echo Start Auction
$baseDir/happy-path/lock-tx.sh 1 0
sleep 2
$baseDir/wait/until-next-block.sh

$baseDir/accounts/log-all-accounts.sh $bn 1
$baseDir/accounts/diff-accounts.sh $bn 0 1

echo Send NFT Back
$baseDir/happy-path/close-start-expired-tx.sh
sleep 2
$baseDir/wait/until-next-block.sh

$baseDir/accounts/log-all-accounts.sh $bn 2
$baseDir/accounts/diff-accounts.sh $bn 1 2
