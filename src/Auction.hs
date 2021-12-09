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
-- import           Control.Monad        hiding (fmap)
import           Codec.Serialise
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS

-- import           Data.Default         (Default (..))
import           Data.Aeson           (ToJSON, FromJSON)
-- import           Data.List.NonEmpty   (NonEmpty (..))
-- import           Data.Text            (pack, Text)
import           GHC.Generics         (Generic)
-- import           Plutus.Contract
-- import           Plutus.Contract.Trace as Trace
-- import           Plutus.ChainIndex.Tx
import qualified PlutusTx             as PlutusTx
import           PlutusTx.Prelude
-- import           PlutusTx.Prelude     (divide)
-- import qualified PlutusTx.Prelude     as Plutus
import           Plutus.V1.Ledger.Credential
import qualified Plutus.V1.Ledger.Scripts as PlutusScripts
import           Ledger               hiding (singleton)
-- import           Ledger.Constraints   as Constraints
import qualified Ledger.Scripts       as Scripts
import qualified Ledger.Typed.Scripts as Scripts hiding (validatorHash)
import           Ledger.Value         as Value
import           Ledger.Ada           as Ada hiding (divide)
-- import           Playground.Contract  (ensureKnownCurrencies, printSchemas, stage, printJson)
-- import           Playground.TH        (mkKnownCurrencies, mkSchemaDefinitions)
-- import           Playground.Types     (KnownCurrency (..))
import           Prelude              (Show (..))
import           Schema               (ToSchema)
-- import           Text.Printf          (printf)
-- import           Plutus.Trace.Emulator as Emulator
-- import           Wallet.Emulator.Wallet

--------------------------------------------------------------------------------------------------
-- On Chain Code
--------------------------------------------------------------------------------------------------
data AuctionDetails = AuctionDetails
    { adSeller             :: !PubKeyHash
    , adCurrency           :: !CurrencySymbol
    , adToken              :: !TokenName
    , adDeadline           :: !POSIXTime
    -- , adBidPercentIncrease :: !Integer --5
    -- , adStartTime          :: !POSIXTime -- 1596000000000 (For the playground only)
    -- , adBidTimeIncrement   :: !POSIXTime -- 172800000 (3 extra 0s because I think milliseconds are included for Plutus
    } deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

instance Eq AuctionDetails where
    {-# INLINABLE (==) #-}
    a == b
      =  (adSeller             a == adSeller                  b)
      && (adCurrency           a == adCurrency                b)
      && (adToken              a == adToken                   b)
      && (adDeadline           a == adDeadline                b)
      -- && (adBidPercentIncrease a == adBidPercentIncrease      b)
      -- && (adStartTime          a == adStartTime               b)
      -- && (adBidTimeIncrement   a == adBidTimeIncrement        b)

PlutusTx.unstableMakeIsData ''AuctionDetails -- Make Stable when live
PlutusTx.makeLift ''AuctionDetails

data BidDetails = BidDetails
    { bdBidder :: !PubKeyHash
    , bdBid    :: !Integer
    -- , bdTime   :: !POSIXTime
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
    , adHighestBid     :: !BidDetails
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
              minBid = bdBid (adHighestBid datum) + 1

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
              = outputDatum == datum { adHighestBid = b }

            oldBidAmount :: Integer
            !oldBidAmount = bdBid . adHighestBid $ datum

            bidDiff :: Integer
            bidDiff = bdBid bd - oldBidAmount

            -- The new value on the script should be the tokenValue
            correctBidOutputValue :: Bool
            correctBidOutputValue =
              txOutValue ownOutput `Value.geq` (actualScriptValue <> Ada.lovelaceValueOf bidDiff)

            correctBidRefund :: Bool
            !correctBidRefund =
              lovelacesPaidTo info (bdBidder $ adHighestBid datum) >= (bdBid $ adHighestBid datum)

            -- Bidding is allowed if the start time is before the tx interval
            -- deadline is later than the valid tx
            -- range. The deadline is in the future.
            correctBidSlotRange :: Bool
            !correctBidSlotRange
              = (adDeadline $ adAuctionDetails datum) `after` txInfoValidRange info

        Close -> False
            --traceIfFalse "Seller must close the auction"                                    isSeller && -- This is required until the time interval bug in plutus 1.30.1 is fixed
            ----traceIfFalse "The auction has not ended yet"                                    (correctSlotRangeCloseEndAuction) &&
            --traceIfFalse "Expected the highest bidder to get the token" (getsValue (bdBidder (adHighestBid datum)) tokenValue) &&
            --traceIfFalse "Expected the sell to get the highest bid" (getsValue (adSeller (adAuctionDetails datum)) (Ada.lovelaceValueOf (bdBid (adHighestBid datum))))
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
        !expectedScriptValue = tokenValue <> Ada.lovelaceValueOf (bdBid $ adHighestBid datum)

        actualScriptValue :: Value
        !actualScriptValue = getOnlyScriptInput info
        -- Ensure the value is on the script address and there is
        -- only one script input.
        correctInputValue :: Bool
        !correctInputValue = actualScriptValue `Value.geq` expectedScriptValue


    --    tokenValue :: Value
    --    tokenValue = Value.singleton (adCurrency (adAuctionDetails datum)) (adToken (adAuctionDetails datum)) 1

    --    ownOutput   :: TxOut
    --    outputDatum :: AuctionDatum
    --    (ownOutput, outputDatum) = case getContinuingOutputs context of
    --        [output] -> case txOutDatumHash output of
    --            Nothing        -> traceError "Wrong output type"
    --            Just datumHash -> case findDatum datumHash txInfo of
    --                Nothing        -> traceError "Datum not found"
    --                Just (Datum d) ->  case PlutusTx.fromBuiltinData d of
    --                    Just auctionDatum -> (output, auctionDatum)
    --                    Nothing           -> traceError "Error decoding data"
    --        _   -> traceError "Expected exactly one continuing output"

    --    isSeller :: Bool
    --    -- isSeller = (cdCloser closeDetails) == (adSeller (adAuctionDetails datum))

    --    --------------------------------------------------------------------------------------------------
    --    -- Time Functions
    --    --------------------------------------------------------------------------------------------------

    --    -- There is a bug in the "to" time intervals in Plutus 1.30.1 where the "to" does not work. using static time for now
    --    --correctSlotRangeBidStartAuction :: Bool
    --    --correctSlotRangeBidStartAuction = from (adStartTime (adAuctionDetails datum)) `contains` txInfoValidRange txInfo

    --    --correctSlotRangeBidEndAuction :: Bool
    --    --correctSlotRangeBidEndAuction = to (deadline datum) `contains` txInfoValidRange txInfo

    --    correctSlotRangeCloseEndAuction :: Bool
    --    correctSlotRangeCloseEndAuction = from (deadline datum) `contains` txInfoValidRange txInfo

    --    --------------------------------------------------------------------------------------------------
    --    -- Data Functions
    --    --------------------------------------------------------------------------------------------------
    --    correctBidOutputDatum :: BidDetails -> Bool
    --    correctBidOutputDatum bid = (adAuctionDetails datum == adAuctionDetails outputDatum)   &&
    --                                (adHighestBid outputDatum == bid)

    --    correctBidOutputValue :: Integer -> Bool
    --    correctBidOutputValue amount = txOutValue ownOutput == tokenValue Plutus.<> Ada.lovelaceValueOf amount

    --    --------------------------------------------------------------------------------------------------
    --    -- Value Functions
    --    --------------------------------------------------------------------------------------------------
    --    sufficientBid :: Integer -> Bool
    --    sufficientBid amount = amount > minBid datum

    --    correctBidRefund :: Bool
    --    correctBidRefund =
    --            let
    --                outputs = [output
    --                          | output <- (txInfoOutputs txInfo)
    --                          , (txOutAddress output) == (pubKeyHashAddress (bdBidder (adHighestBid datum)))
    --                          ]
    --            in
    --                case outputs of
    --                    [output] -> txOutValue output == Ada.lovelaceValueOf (bdBid (adHighestBid datum))
    --                    _        -> traceError "Expected exactly one refund output"

    --    getsValue :: PubKeyHash -> Value -> Bool
    --    getsValue publicKeyHash value =
    --        let
    --            [output] = [ ouput'
    --                       | ouput' <- txInfoOutputs txInfo
    --                       , txOutValue ouput' == value
    --                       ]
    --        in
    --            txOutAddress output == pubKeyHashAddress publicKeyHash

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


