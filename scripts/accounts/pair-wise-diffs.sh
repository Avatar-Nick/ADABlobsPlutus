set -eux
thisDir=$(dirname "$0")

$thisDir/diff-accounts.sh 0 1
$thisDir/diff-accounts.sh 1 2
$thisDir/diff-accounts.sh 2 3
$thisDir/diff-accounts.sh 3 4
$thisDir/diff-accounts.sh 4 5
$thisDir/diff-accounts.sh 5 6
$thisDir/diff-accounts.sh 6 7
$thisDir/diff-accounts.sh 7 8
$thisDir/diff-accounts.sh 9 10
