set -eux
thisDir=$(dirname "$0")
tempDir=$thisDir/../temp

nowSeconds=$(date +%s)
now=$(($nowSeconds*1000))
timestamp=$(($nowSeconds*1000+$1))
prefix=${2:-0}

mkdir -p $tempDir/$BLOCKCHAIN_PREFIX/datums/$prefix
mkdir -p $tempDir/$BLOCKCHAIN_PREFIX/redeemers

sellerPkh=$(cat $tempDir/$BLOCKCHAIN_PREFIX/pkhs/seller-pkh.txt)
marketplacePkh=$(cat $tempDir/$BLOCKCHAIN_PREFIX/pkhs/marketplace-pkh.txt)
royaltyPkh=$(cat $tempDir/$BLOCKCHAIN_PREFIX/pkhs/royalities-pkh.txt)
buyerPkh=$(cat $tempDir/$BLOCKCHAIN_PREFIX/pkhs/buyer-pkh.txt)
buyer1Pkh=$(cat $tempDir/$BLOCKCHAIN_PREFIX/pkhs/buyer1-pkh.txt)

cat << EOF > $tempDir/$BLOCKCHAIN_PREFIX/datums/$prefix/start.json

{
  "constructor": 0,
  "fields": [
    {
      "constructor": 0,
      "fields": [
        {
          "bytes": "$sellerPkh"
        },
        {
          "bytes": "d6cfdbedd242056674c0e51ead01785497e3a48afbbb146dc72ee1e2"
        },
        {
          "bytes": "123456"
        },
        {
          "int": $timestamp
        },
        {
          "int": $now
        },
        {
          "int": 8000000
        },
        {
          "int" : 100
        },
        {
          "bytes": "$marketplacePkh"
        }
      ]
    },
    {
      "constructor": 1,
      "fields": [
      ]
    }
  ]
}

EOF

cat << EOF > $tempDir/$BLOCKCHAIN_PREFIX/datums/$prefix/bid-1.json

{
  "constructor": 0,
  "fields": [
    {
      "constructor": 0,
      "fields": [
        {
          "bytes": "$sellerPkh"
        },
        {
          "bytes": "d6cfdbedd242056674c0e51ead01785497e3a48afbbb146dc72ee1e2"
        },
        {
          "bytes": "123456"
        },
        {
          "int": $timestamp
        },
        {
          "int": $now
        },
        {
          "int": 8000000
        },
        {
          "int" : 100
        },
        {
          "bytes": "$marketplacePkh"
        }
      ]
    },
    {
      "constructor": 0,
      "fields": [
        {
          "constructor": 0,
          "fields": [
            {
              "bytes" : "$buyerPkh"
            },
            {
              "int" : 10000000
            }
          ]
        }
      ]
    }
  ]
}

EOF

cat << EOF > $tempDir/$BLOCKCHAIN_PREFIX/datums/$prefix/seller-bid-1.json
{
  "constructor": 0,
  "fields": [
    {
      "constructor": 0,
      "fields": [
        {
          "bytes": "$sellerPkh"
        },
        {
          "bytes": "d6cfdbedd242056674c0e51ead01785497e3a48afbbb146dc72ee1e2"
        },
        {
          "bytes": "123456"
        },
        {
          "int": $timestamp
        },
        {
          "int": $now
        },
        {
          "int": 8000000
        },
        {
          "int" : 100
        },
        {
          "bytes": "$marketplacePkh"
        }
      ]
    },
    {
      "constructor": 0,
      "fields": [
        {
          "constructor": 0,
          "fields": [
            {
              "bytes" : "$sellerPkh"
            },
            {
              "int" : 10000000
            }
          ]
        }
      ]
    }
  ]
}

EOF

cat << EOF > $tempDir/$BLOCKCHAIN_PREFIX/datums/$prefix/bid-2.json
{
  "constructor": 0,
  "fields": [
    {
      "constructor": 0,
      "fields": [
        {
          "bytes": "$sellerPkh"
        },
        {
          "bytes": "d6cfdbedd242056674c0e51ead01785497e3a48afbbb146dc72ee1e2"
        },
        {
          "bytes": "123456"
        },
        {
          "int": $timestamp
        },
        {
          "int": $now
        },
        {
          "int": 8000000
        },
        {
          "int" : 100
        },
        {
          "bytes": "$marketplacePkh"
        }
      ]
    },
    {
      "constructor": 0,
      "fields": [
        {
          "constructor": 0,
          "fields": [
            {
              "bytes" : "$buyer1Pkh"
            },
            {
              "int" : 30000000
            }
          ]
        }
      ]
    }
  ]
}

EOF

cat << EOF > $tempDir/$BLOCKCHAIN_PREFIX/redeemers/bid-1.json

{
  "constructor": 0,
  "fields": [
    {
      "constructor": 0,
      "fields": [
        {
          "bytes": "$buyerPkh"
        },
        {
          "int": 10000000
        }
      ]
    }
  ]
}


EOF

cat << EOF > $tempDir/$BLOCKCHAIN_PREFIX/redeemers/bid-2.json

{
  "constructor": 0,
  "fields": [
    {
      "constructor": 0,
      "fields": [
        {
          "bytes": "$buyer1Pkh"
        },
        {
          "int": 30000000
        }
      ]
    }
  ]
}

EOF

cat << EOF > $tempDir/$BLOCKCHAIN_PREFIX/redeemers/close.json

{ "constructor":1, "fields": []}

EOF

cat << EOF > $tempDir/$BLOCKCHAIN_PREFIX/redeemers/seller-bid-1.json

{
  "constructor": 0,
  "fields": [
    {
      "constructor": 0,
      "fields": [
        {
          "bytes": "$sellerPkh"
        },
        {
          "int": 10000000
        }
      ]
    }
  ]
}


EOF


$thisDir/hash-datums.sh
