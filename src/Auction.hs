{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE BangPatterns               #-}

module Auction
  (
      auctionScript
    , auctionScriptShortBs
  ) where

import           Cardano.Api.Shelley  (PlutusScript (..), PlutusScriptV1)
import           Codec.Serialise
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import           Data.Aeson           (ToJSON, FromJSON)
import           GHC.Generics         (Generic)
import qualified PlutusTx             as PlutusTx
import           PlutusTx.Prelude
import           Plutus.V1.Ledger.Credential
import qualified Plutus.V1.Ledger.Scripts as PlutusScripts
import           Ledger               hiding (singleton)
import qualified Ledger.Scripts       as Scripts
import qualified Ledger.Typed.Scripts as Scripts hiding (validatorHash)
import           Ledger.Value         as Value
import           Ledger.Ada           as Ada hiding (divide)
import           Prelude              (Show (..))

--------------------------------------------------------------------------------------------------
-- On Chain Code
--------------------------------------------------------------------------------------------------
data AuctionDetails = AuctionDetails
    { adSeller             :: !PubKeyHash
    , adCurrency           :: !CurrencySymbol
    , adToken              :: !TokenName
    , adDeadline           :: !POSIXTime
    , adStartTime          :: !POSIXTime -- 1596000000000 (For the playground only)
    , adMinBid             :: !Integer
    , adMarketplacePercent :: !Integer
    , adMarketplaceAddress :: !PubKeyHash
    } deriving (Show, Generic, ToJSON, FromJSON)

instance Eq AuctionDetails where
    {-# INLINABLE (==) #-}
    a == b
      =  (adSeller             a == adSeller                  b)
      && (adCurrency           a == adCurrency                b)
      && (adToken              a == adToken                   b)
      && (adDeadline           a == adDeadline                b)
      && (adStartTime          a == adStartTime               b)
      && (adMarketplacePercent a == adMarketplacePercent      b)
      && (adMarketplaceAddress a == adMarketplaceAddress      b)
      && (adMinBid             a == adMinBid                  b)

PlutusTx.unstableMakeIsData ''AuctionDetails -- Make Stable when live
PlutusTx.makeLift ''AuctionDetails

data BidDetails = BidDetails
    { bdBidder :: !PubKeyHash
    , bdBid    :: !Integer
    } deriving Show

instance Eq BidDetails where
    {-# INLINABLE (==) #-}
    a == b
      =  (bdBidder a == bdBidder b)
      && (bdBid    a == bdBid    b)

PlutusTx.unstableMakeIsData ''BidDetails -- Make Stable when live
PlutusTx.makeLift ''BidDetails

data AuctionDatum = AuctionDatum
    { adAuctionDetails :: !AuctionDetails
    , adHighestBid     :: !(Maybe BidDetails)
    } deriving Show

instance Eq AuctionDatum where
    {-# INLINABLE (==) #-}
    a == b
      =  (adAuctionDetails a == adAuctionDetails b)
      && (adHighestBid     a == adHighestBid     b)

PlutusTx.unstableMakeIsData ''AuctionDatum -- Make Stable when live
PlutusTx.makeLift ''AuctionDatum

data AuctionRedeemer = Bid BidDetails | Close
    deriving Show

PlutusTx.unstableMakeIsData ''AuctionRedeemer -- Make Stable when live
PlutusTx.makeLift ''AuctionRedeemer

data Auctioning
instance Scripts.ValidatorTypes Auctioning where
    type instance DatumType Auctioning = AuctionDatum
    type instance RedeemerType Auctioning = AuctionRedeemer

-- Returns an Integer whose value is 'percent' percent greater than input (rounded down)
{-# INLINABLE increasePercent #-}
increasePercent :: Integer -> Integer -> Integer
increasePercent input percent = (input * (100 + percent)) `divide` 100

{-# INLINABLE lovelaces #-}
lovelaces :: Value -> Integer
lovelaces = Ada.getLovelace . Ada.fromValue

{-# INLINABLE lovelacesPaidTo #-}
lovelacesPaidTo :: TxInfo -> PubKeyHash -> Integer
lovelacesPaidTo info pkh = lovelaces (valuePaidTo info pkh)


-------------------------------------------------------------------------------
-- Payout Utilities
-------------------------------------------------------------------------------
type Percent = Integer
type Lovelaces = Integer

{-# INLINABLE minAda #-}
minAda :: Lovelaces
minAda = 1_000_000

{-# INLINABLE applyPercent #-}
applyPercent :: Integer -> Lovelaces -> Percent -> Lovelaces
applyPercent divider inVal pct = (inVal * pct) `divide` divider

{-# INLINABLE payoutIsValid #-}
payoutIsValid :: Lovelaces -> TxInfo -> PubKeyHash -> PubKeyHash -> Percent -> Bool
payoutIsValid total info seller marketplace percent =
  let
    marketPlaceAmount = if percent == 0
      then 0
      else max minAda ((total * percent) `divide` 1000)

    sellerAmount = total - marketPlaceAmount

  in traceIfFalse "Marketplace not paid!"
      (lovelacesPaidTo info marketplace >= marketPlaceAmount)
  && traceIfFalse "Marketplace not paid!"
      (lovelacesPaidTo info seller >= sellerAmount)


-------------------------------------------------------------------------------
{-

Batch Transaction Exploit Protection

All combinations of redeemers
outbid/outbid, close/close and close,outbid
are exploitable.

If an attacker outbids the same payout address twice,
they could rewire one of the bid refunds to themselves.

If an attacker outbids and closes with the same payout address,
they could rewire half the refunds/payouts.

If an attacker double closes with the same payout address,
they could rewire half the payouts.

-}
-------------------------------------------------------------------------------
{-# INLINABLE isScriptAddress #-}
isScriptAddress :: Address -> Bool
isScriptAddress Address { addressCredential } = case addressCredential of
  ScriptCredential _ -> True
  _ -> False

-- Verify that there is only one script input and get it's value.
{-# INLINABLE getOnlyScriptInput #-}
getOnlyScriptInput :: TxInfo -> Value
getOnlyScriptInput info =
  let
    isScriptInput :: TxInInfo -> Bool
    isScriptInput = isScriptAddress . txOutAddress . txInInfoResolved

    input = case filter isScriptInput . txInfoInputs $ info of
      [i] -> i
      _ -> traceError "expected exactly one script input"

  in txOutValue . txInInfoResolved $ input

{-# INLINABLE mkAuctionValidator #-}
mkAuctionValidator :: AuctionDatum -> AuctionRedeemer -> ScriptContext -> Bool
mkAuctionValidator datum redeemer ctx =
  -- Always perform the input check
  traceIfFalse "wrong input value" correctInputValue
    && case redeemer of
        Bid bd ->
          traceIfFalse "bid too low"        (sufficientBid $ bdBid bd)
            && traceIfFalse "wrong output datum" (correctBidOutputDatum bd)
            && traceIfFalse "wrong output value" correctBidOutputValue
            && traceIfFalse "wrong refund"       correctBidRefund
            && traceIfFalse "too late"           correctBidSlotRange
          where
            -- Ensure the amount is great than the current
            -- min bid, e.g. the reserve price or last bid.
            sufficientBid :: Integer -> Bool
            sufficientBid amount = amount >= minBid where
              minBid = maybe (adMinBid $ adAuctionDetails datum) bdBid (adHighestBid datum) + 1

            ownOutput   :: TxOut
            outputDatum :: AuctionDatum

            (!ownOutput, !outputDatum) = case getContinuingOutputs ctx of
              [o] -> case txOutDatumHash o of
                Nothing -> traceError "wrong output type"
                Just h -> case findDatum h info of
                  Nothing -> traceError "datum not found"
                  Just (Datum d) ->  case PlutusTx.fromBuiltinData d of
                    Just !ad' -> (o, ad')
                    Nothing  -> traceError "error decoding data"
              _ -> traceError "expected exactly one continuing output"

            -- Make sure we are setting the next datum correctly
            -- Everything should be the same, but we should
            -- update the latest bid.
            correctBidOutputDatum :: BidDetails -> Bool
            correctBidOutputDatum b
              = outputDatum == datum { adHighestBid = Just b }

            oldBidAmount :: Integer
            !oldBidAmount = maybe 0 bdBid . adHighestBid $ datum

            bidDiff :: Integer
            bidDiff = bdBid bd - oldBidAmount

            -- The new value on the script should be the tokenValue
            correctBidOutputValue :: Bool
            correctBidOutputValue =
              txOutValue ownOutput `Value.geq` (actualScriptValue <> Ada.lovelaceValueOf bidDiff)

            correctBidRefund :: Bool
            !correctBidRefund = case adHighestBid datum of
              Nothing -> True
              Just b -> lovelacesPaidTo info (bdBidder b) >= (bdBid b)

            -- Bidding is allowed if the start time is before the tx interval
            -- deadline is later than the valid tx
            -- range. The deadline is in the future.
            correctBidSlotRange :: Bool
            !correctBidSlotRange
              =  (adDeadline $ adAuctionDetails datum) `after` txInfoValidRange info
              && (adStartTime $ adAuctionDetails datum) `before` txInfoValidRange info

        Close ->
          let
            -- Closing is allowed if the deadline is before than the valid tx
            -- range. The deadline is past.
            correctCloseSlotRange :: Bool
            !correctCloseSlotRange = (adDeadline $ adAuctionDetails datum) `before` txInfoValidRange info

            in traceIfFalse "too early" correctCloseSlotRange
            && case adHighestBid datum of
                Nothing
                  -> traceIfFalse
                      "expected seller to get token"
                      (getsValue (adSeller $ adAuctionDetails datum) tokenValue)
                Just BidDetails{..}
                  -> traceIfFalse
                      "expected highest bidder to get token"
                      (getsValue bdBidder tokenValue)
                  && payoutIsValid bdBid info
                        (adSeller $ adAuctionDetails datum)
                        (adMarketplaceAddress $ adAuctionDetails datum)
                        (adMarketplacePercent $ adAuctionDetails datum)

    where
    --    --------------------------------------------------------------------------------------------------
    --    -- Helper Functions
    --    --------------------------------------------------------------------------------------------------
        info :: TxInfo
        info = scriptContextTxInfo ctx

        -- The asset we are auctioning as a Value
        tokenValue :: Value
        tokenValue = Value.singleton (adCurrency $ adAuctionDetails datum) (adToken $ adAuctionDetails datum) 1

        -- The value we expect on the script input based on
        -- datum.
        expectedScriptValue :: Value
        !expectedScriptValue = tokenValue <> Ada.lovelaceValueOf (maybe 0 bdBid $ adHighestBid datum)

        actualScriptValue :: Value
        !actualScriptValue = getOnlyScriptInput info
        -- Ensure the value is on the script address and there is
        -- only one script input.
        correctInputValue :: Bool
        !correctInputValue = actualScriptValue `Value.geq` expectedScriptValue

        -- Helper to make sure the pkh is paid at least the value.
        getsValue :: PubKeyHash -> Value -> Bool
        getsValue h v = valuePaidTo info h `Value.geq` v


auctionTypedValidator :: Scripts.TypedValidator Auctioning
auctionTypedValidator = Scripts.mkTypedValidator @Auctioning
    $$(PlutusTx.compile [|| mkAuctionValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @AuctionDatum @AuctionRedeemer

auctionValidator :: Validator
auctionValidator = Scripts.validatorScript auctionTypedValidator

auctionAddress :: Ledger.ValidatorHash
auctionAddress = Scripts.validatorHash auctionValidator

script :: PlutusScripts.Script
script = PlutusScripts.unValidatorScript auctionValidator

auctionScriptShortBs :: SBS.ShortByteString
auctionScriptShortBs = SBS.toShort . LBS.toStrict $ serialise script

auctionScript :: PlutusScript PlutusScriptV1
auctionScript = PlutusScriptSerialised auctionScriptShortBs
