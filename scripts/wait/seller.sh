set -eux
thisDir=$(dirname "$0")
baseDir=$thisDir/../

$thisDir/wait-until-next-block.sh
$baseDir/query/seller.sh
