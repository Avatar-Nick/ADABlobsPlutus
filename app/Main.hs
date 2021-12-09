{-# LANGUAGE RecordWildCards #-}

module Main where

import Cardano.Api hiding (TxId)
import Options.Applicative

import Auction

data Opts = Opts
  { output :: FilePath
  } deriving Show

main :: IO ()
main = createSC =<< execParser opts

opts :: ParserInfo Opts
opts = info (optsParser <**> helper) . mconcat $
  [ fullDesc
  , progDesc "Create a smart contract for auctions"
  ]

optsParser :: Parser Opts
optsParser = Opts
  <$> (strOption . mconcat $
    [ long "output"
    , metavar "FILE"
    , help "Where to write the Plutus script"
    , value "scripts/auction.plutus"
    ])

createSC :: Opts -> IO ()
createSC Opts{..} = do
  result <- writeFileTextEnvelope output Nothing auctionScript
  case result of
      Left err -> print $ displayError err
      Right () -> putStrLn $ "wrote validator to file " ++ output
