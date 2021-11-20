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

module TestTransaction 
    (
        testTransactionScript,
        testTransactionScriptShortBs,
        testTransaction
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
data TestTransactionDatum = TestTransactionDatum
    { giver :: PubKeyHash
    } deriving Show

PlutusTx.unstableMakeIsData ''TestTransactionDatum -- Make Stable when live
PlutusTx.makeLift ''TestTransactionDatum

data GrabDetails = GrabDetails 
    {
        grabber :: PubKeyHash
    } deriving Show

instance Eq GrabDetails where
    {-# INLINABLE (==) #-}
    a == b = (grabber a == grabber b)

PlutusTx.unstableMakeIsData ''GrabDetails -- Make Stable when live
PlutusTx.makeLift ''GrabDetails

data TestTransactionRedeemer = Grab GrabDetails
    deriving Show

PlutusTx.unstableMakeIsData ''TestTransactionRedeemer -- Make Stable when live
PlutusTx.makeLift ''TestTransactionRedeemer

{-# INLINABLE mkTestTransactionValidator #-}
mkTestTransactionValidator :: TestTransactionDatum -> TestTransactionRedeemer -> ScriptContext -> Bool
mkTestTransactionValidator _ _ _ = True

-- Boiler Plate Validation Functions
data TestTransaction
instance Scripts.ValidatorTypes TestTransaction where
    type instance DatumType TestTransaction = TestTransactionDatum
    type instance RedeemerType TestTransaction = TestTransactionRedeemer

testTransactionTypedValidator :: Scripts.TypedValidator TestTransaction
testTransactionTypedValidator = Scripts.mkTypedValidator @TestTransaction
    $$(PlutusTx.compile [|| mkTestTransactionValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @TestTransactionDatum @TestTransactionRedeemer

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

--------------------------------------------------------------------------------------------------
-- Off Chain Code
--------------------------------------------------------------------------------------------------
type TestTransactionSchema = 
        Endpoint "give" Integer
    .\/ Endpoint "grab" ()

give :: AsContractError e => Promise () TestTransactionSchema e ()
give = endpoint @"give" @Integer $ \(amount) -> do
    logInfo @String (printf "Starting give endpoint")
    
    pkh <- Plutus.Contract.ownPubKeyHash
    let testTxDatum = TestTransactionDatum
                        {
                            giver = pkh
                        }    
        tx          = Constraints.mustPayToTheScript testTxDatum (Ada.lovelaceValueOf amount)
    void (submitTxConstraints testTransactionTypedValidator tx)
    logInfo @String (printf "Ending give endpoint")

grab :: AsContractError e => Promise () TestTransactionSchema e ()
grab = endpoint @"grab" @() $ \(()) -> do
    logInfo @String (printf "Starting grab endpoint")
    utxos <- utxosAt scrAddress
    pkh <- Plutus.Contract.ownPubKeyHash
    let grabDetails = GrabDetails 
                        {
                            grabber = pkh
                        }
        redeemer   = Redeemer (PlutusTx.toBuiltinData (Grab grabDetails))
        outputRefs = fst <$> Map.toList utxos
        lookups    = Constraints.typedValidatorLookups testTransactionTypedValidator <>
                     Constraints.otherScript testTransactionValidator <>
                     Constraints.unspentOutputs utxos
        tx         = mconcat [mustSpendScriptOutput outputRef redeemer | outputRef <- outputRefs]
    
    void (submitTxConstraintsWith lookups tx)
    logInfo @String (printf "Ending grab endpoint")

contract :: AsContractError e => Contract () TestTransactionSchema e ()
contract = selectList [give, grab]

endpoints :: AsContractError e => Contract () TestTransactionSchema e ()
endpoints = contract

mkSchemaDefinitions ''TestTransactionSchema

mkKnownCurrencies []

--------------------------------------------------------------------------------------------------
-- Test Code
--------------------------------------------------------------------------------------------------
testTransaction :: IO()
testTransaction = Emulator.runEmulatorTraceIO' def emulatorConfig myTrace

emulatorConfig :: EmulatorConfig
emulatorConfig = EmulatorConfig (Left $ Map.fromList [(Trace.knownWallet i, v) | i <- [1 .. 3]]) def def
    where
        v :: Value
        v = Ada.lovelaceValueOf 100000000

myTrace :: EmulatorTrace()
myTrace = do
    let w1 = Trace.knownWallet 1
        w2 = Trace.knownWallet 2
        w3 = Trace.knownWallet 3

    hW1Give <- Emulator.activateContractWallet w1 (give @ContractError)
    void $ Emulator.waitNSlots 1

    hW2Grab <- Emulator.activateContractWallet w2 (grab @ContractError)
    void $ Emulator.waitNSlots 1

    Emulator.callEndpoint @"give" hW1Give $ 100000
    void $ Emulator.waitNSlots 1

    Emulator.callEndpoint @"grab" hW2Grab $ ()
    void $ Emulator.waitNSlots 1