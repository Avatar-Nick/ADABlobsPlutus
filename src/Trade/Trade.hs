{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE BangPatterns               #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Trade where

import           Control.Monad        hiding (fmap)
import           Data.Aeson           (ToJSON, FromJSON)
import           Data.List.NonEmpty   (NonEmpty (..))
import           Data.Map             as Map
import           Data.Text            (pack, Text)
import           GHC.Generics         (Generic)
import           Plutus.Contract
import           Plutus.ChainIndex.Tx
import qualified PlutusTx             as PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup(..), unless)
import qualified PlutusTx.Prelude     as Plutus
import           Ledger               hiding (singleton)
import           Ledger.Constraints   as Constraints
import qualified Ledger.Scripts       as Scripts
import qualified Ledger.Typed.Scripts as Scripts hiding (validatorHash)
import           Ledger.Value         as Value
import           Ledger.Ada           as Ada hiding (divide)
import           Playground.Contract  (ensureKnownCurrencies, printSchemas, stage, printJson)
import           Playground.TH        (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types     (KnownCurrency (..))
import           Prelude              (IO, Semigroup (..), Show (..), String)
import           Schema               (ToSchema)
import           Text.Printf          (printf)

--------------------------------------------------------------------------------------------------
-- On Chain Code
--------------------------------------------------------------------------------------------------
-- Defining the Bid datatype and the equivalence criteria
data BuyDetails = BuyDetails
    { bdBuyer :: !PubKeyHash
    , bdPrice :: !Integer
    } deriving Show

instance Eq BuyDetails where
    {-# INLINABLE (==) #-}
    a == b = (bdBuyer a == bdBuyer b) &&
             (bdPrice    a == bdPrice    b)

PlutusTx.unstableMakeIsData ''BuyDetails
PlutusTx.makeLift ''BuyDetails

data BuySellDatum = BuySellDatum 
    { bsSeller   :: !PubKeyHash
    , bsCurrency :: !CurrencySymbol
    , bsToken    :: !TokenName
    , bsPrice    :: !Integer
    } deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

instance Eq BuySellDatum where 
    {-# INLINABLE (==) #-}
    a == b = (bsSeller              a == bsSeller              b) &&
             (bsCurrency            a == bsCurrency            b) &&
             (bsToken               a == bsToken               b) &&  
             (bsPrice               a == bsPrice               b)

PlutusTx.unstableMakeIsData ''BuySellDatum
PlutusTx.makeLift ''BuySellDatum

-- Redeemer is an action (need to add cancel)
data BuySellRedeemer = Buy BuyDetails | Sell
    deriving Show

PlutusTx.unstableMakeIsData ''BuySellRedeemer
PlutusTx.makeLift ''BuySellRedeemer

{-# INLINABLE enoughToBuy #-}
enoughToBuy :: Integer -> Integer -> Bool
enoughToBuy buyPrice sellPrice = buyPrice >= sellPrice

{-# INLINABLE mkBuySellValidator #-}
mkBuySellValidator :: BuySellDatum -> BuySellRedeemer -> ScriptContext -> Bool
mkBuySellValidator datum redeemer context =
    case redeemer of
        Buy buyDetails@BuyDetails{..} ->
            traceIfFalse "Not enough to buy" (enoughToBuy bdPrice (bsPrice datum)) &&
            traceIfFalse "Expected buyer to get token" (getsValue bdBuyer tokenValue) &&
            traceIfFalse "Expected seller to get currency" (getsValue (bsSeller datum) (Ada.lovelaceValueOf (bsPrice datum)))
    where
        txInfo :: TxInfo
        txInfo = scriptContextTxInfo context

        tokenValue :: Value
        tokenValue = Value.singleton (bsCurrency datum) (bsToken datum) 1

        getsValue :: PubKeyHash -> Value -> Bool
        getsValue pubKeyHash value =
            let
                [output] = [output' | output' <- txInfoOutputs txInfo, txOutValue output' == value]
            in
                txOutAddress output == pubKeyHashAddress pubKeyHash      

-- Boiler Plate Validation Functions
data BuySell
instance Scripts.ValidatorTypes BuySell where
    type instance DatumType BuySell = BuySellDatum
    type instance RedeemerType BuySell = BuySellRedeemer

buySellTypedValidator :: Scripts.TypedValidator BuySell
buySellTypedValidator = Scripts.mkTypedValidator @BuySell
    $$(PlutusTx.compile [|| mkBuySellValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @BuySellDatum @BuySellRedeemer

buySellValidator :: Validator
buySellValidator = Scripts.validatorScript buySellTypedValidator

buySellAddress :: Ledger.ValidatorHash
buySellAddress = Scripts.validatorHash buySellValidator

--------------------------------------------------------------------------------------------------
-- Off Chain Code
--------------------------------------------------------------------------------------------------
data BuySellParams = BuySellParams
    { currency :: !CurrencySymbol
    , token    :: !TokenName
    , price    :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

type BuySellSchema = 
        Endpoint "sell" BuySellParams
    .\/ Endpoint "buy"  BuySellParams

buy :: AsContractError e => Promise () BuySellSchema e ()
buy = endpoint @"buy" @BuySellParams $ \(BuySellParams{..}) -> do
    logInfo @String (printf "Starting buy endpoint")
    (outputReference, chainTxOut, buySellDatum@BuySellDatum{..}) <- findSale currency token
    logInfo @String (printf "Found buy / sell utxo with datum %s" (show buySellDatum))

    if not (enoughToBuy price (bsPrice)) then traceError "Not enough data to buy"
    else do
        pkh <- Plutus.Contract.ownPubKeyHash
        let buyDetails = BuyDetails {bdBuyer = pkh, bdPrice = price}
            redeemer   = Redeemer (PlutusTx.toBuiltinData (Buy buyDetails))
            value      = Ada.lovelaceValueOf bsPrice
            lookups    = Constraints.typedValidatorLookups buySellTypedValidator <>
                         Constraints.otherScript buySellValidator                <>
                         Constraints.unspentOutputs (Map.singleton outputReference chainTxOut)
            tx         = mustPayToPubKey bsSeller value <>
                         mustSpendScriptOutput outputReference redeemer 
        --ledgerTx <- submitTxConstraintsWith lookups tx
        --void (awaitTxConfirmed (txId ledgerTx))
        submitTxConstraintsWith lookups tx
        logInfo @String $ printf "Buying token %s for %s" (show token) (show bsPrice)

sell :: AsContractError e => Promise () BuySellSchema e ()
sell = endpoint @"sell" @BuySellParams $ \(BuySellParams{..}) -> do
    logInfo @String (printf "Starting sell endpoint")

    pkh <- Plutus.Contract.ownPubKeyHash
    let buySellDatum = BuySellDatum
                           { bsSeller     = pkh
                           , bsCurrency   = currency
                           , bsToken      = token
                           , bsPrice      = price
                           }
        value        = Value.singleton currency token 1
        tx           = Constraints.mustPayToTheScript buySellDatum value                      
    --ledgerTx <- submitTxConstraints buySellTypedValidator tx
    --void (awaitTxConfirmed (txId ledgerTx))
    submitTxConstraints buySellTypedValidator tx
    logInfo @String $ printf "Selling token %s for %s" (show token) (show price)

findSale :: AsContractError e => CurrencySymbol -> TokenName -> Contract w s e (TxOutRef, ChainIndexTxOut, BuySellDatum)
findSale currencySymbol tokenName = do
    utxos <- utxosTxOutTxAt (scriptAddress buySellValidator)
    let xs = [ (outputReference, (chainTxOut, chainTx))
             | (outputReference, (chainTxOut, chainTx)) <- Map.toList utxos
             , Value.valueOf (txOutValue (toTxOut chainTxOut)) currencySymbol tokenName == 1             
             ]
    case xs of
        [(outputReference, (chainTxOut, chainTx))] -> case txOutDatumHash (toTxOut chainTxOut) of
            Nothing        -> traceError "Unexpected out type"
            Just datumHash -> case Map.lookup datumHash (getDatumMap chainTx) of
                Nothing        -> traceError "Datum not found"
                Just (Datum e) -> case PlutusTx.fromBuiltinData e of
                    Nothing -> traceError "Datum has wrong type"
                    Just datum@BuySellDatum{..}
                        | bsCurrency == currencySymbol && bsToken == tokenName -> return (outputReference, chainTxOut, datum)
                        | otherwise                                                  -> traceError "Buy sell token missmatch"
        _                     -> traceError "Buy Sell utxo not found" 

getDatumMap :: ChainIndexTx -> Map DatumHash Datum
getDatumMap chainTx = _citxData chainTx

contract :: AsContractError e => Contract () BuySellSchema e ()
contract = selectList [sell, buy]

endpoints :: AsContractError e => Contract () BuySellSchema e ()
endpoints = contract

mkSchemaDefinitions ''BuySellSchema

-- The unCurrencySymbol is 66, unTokenName is T
myToken :: KnownCurrency
myToken = KnownCurrency (ValidatorHash "f") "Token" (TokenName "T" :| [])

mkKnownCurrencies ['myToken]