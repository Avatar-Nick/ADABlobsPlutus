set -eux
thisDir=$(dirname "$0")
baseDir=$thisDir/../
tempDir=$thisDir/../../temp

namespace=$1
index=$2
outputDir=$tempDir/accounts/$namespace/$index

mkdir -p $outputDir

$thisDir/attacker.sh > $outputDir/attacker.json
$thisDir/buyer-1.sh > $outputDir/buyer-1.json
$thisDir/buyer.sh > $outputDir/buyer.json
$thisDir/marketplace.sh > $outputDir/marketplace.json
$thisDir/royalties.sh > $outputDir/royalties.json
$thisDir/sc.sh > $outputDir/sc.json
$thisDir/seller.sh > $outputDir/seller.json
