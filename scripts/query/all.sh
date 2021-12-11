set -eu
thisDir=$(dirname "$0")
baseDir=$thisDir/../

echo "seller"
$thisDir/seller.sh
echo "\nsc"
$thisDir/sc.sh | tail -n +2
echo "\nbuyer"
$thisDir/buyer.sh | tail -n +2
echo "\nbuyer-1"
$thisDir/buyer-1.sh | tail -n +2
echo "\nmarketplace"
$thisDir/marketplace.sh | tail -n +2
echo "\nattacker"
$thisDir/attacker.sh | tail -n +2
