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

module TestBasicTransaction 
    (
        testTransactionScript,
        testTransactionScriptShortBs
    ) where

import           Cardano.Api.Shelley  (PlutusScript (..), PlutusScriptV1)
import           Control.Monad        hiding (fmap)
import           Codec.Serialise
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS

import           Data.Default         (Default (..))
import           Data.Aeson           (ToJSON, FromJSON)
import           Data.List.NonEmpty   (NonEmpty (..))
import           Data.Map             as Map
import           Data.Text            (pack, Text)
import           GHC.Generics         (Generic)
import           Plutus.Contract
import           Plutus.Contract.Trace as Trace
import           Plutus.ChainIndex.Tx
import qualified PlutusTx             as PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup(..), unless)
import qualified PlutusTx.Prelude     as Plutus
import qualified Plutus.V1.Ledger.Scripts as PlutusScripts
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
import           Plutus.Trace.Emulator as Emulator
import           Wallet.Emulator.Wallet

--------------------------------------------------------------------------------------------------
-- On Chain Code
--------------------------------------------------------------------------------------------------
{-# INLINABLE mkTestTransactionValidator #-}
mkTestTransactionValidator :: BuiltinData -> () -> ScriptContext -> Bool
mkTestTransactionValidator _ _ _ = True

-- Boiler Plate Validation Functions
data TestTransaction
instance Scripts.ValidatorTypes TestTransaction where
    type instance DatumType TestTransaction = BuiltinData
    type instance RedeemerType TestTransaction = ()

testTransactionTypedValidator :: Scripts.TypedValidator TestTransaction
testTransactionTypedValidator = Scripts.mkTypedValidator @TestTransaction
    $$(PlutusTx.compile [|| mkTestTransactionValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @BuiltinData @()

testTransactionValidator :: Validator
testTransactionValidator = Scripts.validatorScript testTransactionTypedValidator

testTransactionHash :: Ledger.ValidatorHash
testTransactionHash = Scripts.validatorHash testTransactionValidator

scrAddress :: Ledger.Address
scrAddress = scriptAddress testTransactionValidator

-- Output Section
script :: PlutusScripts.Script
script = PlutusScripts.unValidatorScript testTransactionValidator

testTransactionScriptShortBs :: SBS.ShortByteString
testTransactionScriptShortBs = SBS.toShort . LBS.toStrict $ serialise script

testTransactionScript :: PlutusScript PlutusScriptV1
testTransactionScript = PlutusScriptSerialised testTransactionScriptShortBs
