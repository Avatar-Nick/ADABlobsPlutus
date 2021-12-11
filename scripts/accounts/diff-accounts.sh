set -eux
thisDir=$(dirname "$0")
baseDir=$thisDir/../
tempDir=$thisDir/../../temp

namespace=$1
oldIndex=$2
newIndex=$3
oldDir=$tempDir/accounts/$namespace/$oldIndex
newDir=$tempDir/accounts/$namespace/$newIndex

diffDir=$tempDir/accounts/diffs/$namespace/$oldIndex-$newIndex
mkdir -p $diffDir

diff=$(cardano-cli-balance-fixer diff-values --value-1 $oldDir/attacker.json --value-2 $newDir/attacker.json)
if [ $diff != '{}' ]; then
  echo $diff > $diffDir/attacker.json
fi

diff=$(cardano-cli-balance-fixer diff-values --value-1 $oldDir/buyer-1.json --value-2 $newDir/buyer-1.json)
if [ $diff != '{}' ]; then
  echo $diff > $diffDir/buyer-1.json
fi

diff=$(cardano-cli-balance-fixer diff-values --value-1 $oldDir/buyer.json --value-2 $newDir/buyer.json)
if [ $diff != '{}' ]; then
  echo $diff > $diffDir/buyer.json
fi

diff=$(cardano-cli-balance-fixer diff-values --value-1 $oldDir/marketplace.json --value-2 $newDir/marketplace.json)
if [ $diff != '{}' ]; then
  echo $diff > $diffDir/marketplace.json
fi

diff=$(cardano-cli-balance-fixer diff-values --value-1 $oldDir/royalties.json --value-2 $newDir/royalties.json)
if [ $diff != '{}' ]; then
  echo $diff > $diffDir/royalties.json
fi

diff=$(cardano-cli-balance-fixer diff-values --value-1 $oldDir/sc.json --value-2 $newDir/sc.json)
if [ $diff != '{}' ]; then
  echo $diff > $diffDir/sc.json
fi

diff=$(cardano-cli-balance-fixer diff-values --value-1 $oldDir/seller.json --value-2 $newDir/seller.json)
if [ $diff != '{}' ]; then
  echo $diff > $diffDir/seller.json
fi
