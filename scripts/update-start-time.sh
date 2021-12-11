set -eux
thisDir=$(dirname "$0")

nowSeconds=$(date +%s)
now=$(($nowSeconds*1000))
timestamp=$(($nowSeconds*1000+$1))
prefix=${2:-0}

mkdir -p $thisDir/$BLOCKCHAIN_PREFIX/datums/$prefix

sellerPkh=$(cat $thisDir/$BLOCKCHAIN_PREFIX/pkhs/seller-pkh.txt)
marketplacePkh=$(cat $thisDir/$BLOCKCHAIN_PREFIX/pkhs/marketplace-pkh.txt)
royaltyPkh=$(cat $thisDir/$BLOCKCHAIN_PREFIX/pkhs/royalities-pkh.txt)
buyerPkh=$(cat $thisDir/$BLOCKCHAIN_PREFIX/pkhs/buyer-pkh.txt)
buyer1Pkh=$(cat $thisDir/$BLOCKCHAIN_PREFIX/pkhs/buyer1-pkh.txt)

cat << EOF > $thisDir/$BLOCKCHAIN_PREFIX/datums/$prefix/start.json
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
          "map": [
            {
              "v": {
                "int": 900
              },
              "k": {
                "bytes": "$sellerPkh"
              }
            },
            {
              "v": {
                "int": 50
              },
              "k": {
                "bytes": "$royaltyPkh"
              }
            },
            {
              "v": {
                "int": 50
              },
              "k": {
                "bytes": "$marketplacePkh"
              }
            }
          ]
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

cat << EOF > $thisDir/$BLOCKCHAIN_PREFIX/datums/$prefix/bid-1.json
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
          "map": [
            {
              "v": {
                "int": 900
              },
              "k": {
                "bytes": "$sellerPkh"
              }
            },
            {
              "v": {
                "int": 50
              },
              "k": {
                "bytes": "$royaltyPkh"
              }
            },
            {
              "v": {
                "int": 50
              },
              "k": {
                "bytes": "$marketplacePkh"
              }
            }
          ]
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

cat << EOF > $thisDir/$BLOCKCHAIN_PREFIX/datums/$prefix/seller-bid-1.json
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
          "map": [
            {
              "v": {
                "int": 900
              },
              "k": {
                "bytes": "$sellerPkh"
              }
            },
            {
              "v": {
                "int": 50
              },
              "k": {
                "bytes": "$royaltyPkh"
              }
            },
            {
              "v": {
                "int": 50
              },
              "k": {
                "bytes": "$marketplacePkh"
              }
            }
          ]
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

cat << EOF > $thisDir/$BLOCKCHAIN_PREFIX/datums/$prefix/bid-2.json
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
          "map": [
            {
              "v": {
                "int": 900
              },
              "k": {
                "bytes": "$sellerPkh"
              }
            },
            {
              "v": {
                "int": 50
              },
              "k": {
                "bytes": "$royaltyPkh"
              }
            },
            {
              "v": {
                "int": 50
              },
              "k": {
                "bytes": "$marketplacePkh"
              }
            }
          ]
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

$thisDir/hash-datums.sh
