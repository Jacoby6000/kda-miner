{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}

module Miner.Types
  ( -- * Runtime Environment
    Env(..)
    -- * CLI Flags
  , ClientArgs(..)
  , pCommand
  , Command(..)
  , GPUEnv(..)
  , OtherCommand(..)
  , GPUDevice(..)
  , HostAddress(..)
  , Miner(..)
    -- * miscellaneous
  , tlsSettings
  , donateTo
  , showT
  , round2
  , roundN
  , validatePKey
  ) where

import           Data.Aeson 
import           Data.Generics.Product.Fields (field)
import           Data.List
import           Data.List.Split
import qualified Data.Text as T
import           Data.Time.Clock.POSIX (POSIXTime)
import           Data.Tuple.Strict (T2(..))
import           Network.Connection
import           Network.HTTP.Client hiding (Proxy(..), responseBody, port, host)
import           Options.Applicative
import           RIO as R hiding (fromEitherM)
import           RIO.Char (isHexDigit)
import qualified System.Random.MWC as MWC

data Env = Env
    { envGen         :: !MWC.GenIO
    , envMgr         :: !Manager
    , envLog         :: !LogFunc
    , envGpu         :: !GPUEnv
    , envArgs        :: !ClientArgs
    , envHashes      :: [IORef Word64]
    , envSecs        :: IORef Word64
    , envLastSuccess :: IORef POSIXTime
    , envUrls        :: IORef (NonEmpty (T2 HostAddress Text)) }
    deriving stock (Generic)

instance HasLogFunc Env where
    logFuncL = field @"envLog"

--------------------------------------------------------------------------------
-- CLI Flags

-- | Result of parsing commandline flags.

data HostAddress = HostAddress
  { addressHostname :: Text
  , addressPort :: Int
  }

data ClientArgs = ClientArgs
    { ll           :: !LogLevel
    , coordinators :: ![HostAddress]
    , miner        :: !Miner
    }
    deriving stock (Generic)

data Miner = Miner
  { account :: Text
  , publicKeys :: [Text]
  , keyPred :: Text
  } deriving stock (Generic)

instance ToJSON Miner where
  toJSON (Miner acct keys pred) = object ["account" .= acct, "public-keys" .= keys, "predicate" .= pred]

-- | The top-level git-style CLI "command" which determines which mining
-- paradigm to follow.
--
data Command = GPU GPUEnv ClientArgs | Otherwise OtherCommand

data GPUEnv = GPUEnv 
    { gpuDevices :: [GPUDevice] 
    , globalSize :: Integer
    , localSize :: Integer
    , workSetSize :: Integer
    } deriving stock (Generic)

data GPUDevice = GPUDevice
  { platformIndex :: Int
  , deviceIndex :: Int
  } deriving stock (Generic)

pClientArgs :: Parser ClientArgs
pClientArgs = ClientArgs <$> pLog <*> some pUrl <*> pMiner 

pCommand :: Parser Command
pCommand = hsubparser
    (  command "mine" (info gpuOpts (progDesc "Perform mining"))
    <> command "show-devices" (info (Otherwise <$> showDevicesOpts) (progDesc "Show all available OpenCL Devices"))
    )

pGpuEnv :: Parser GPUEnv
pGpuEnv = GPUEnv <$> many pDevice <*> pGlobalSize <*> pLocalSize <*> pWorkSetSize  

pGlobalSize :: Parser Integer
pGlobalSize = option readOption
    (short 'g' <> long "global-size" <> help "OpenCL global work size (default 1024*1024*16)" <> value (1024*1024*16))

pLocalSize :: Parser Integer
pLocalSize = option readOption
    (short 'l' <> long "local-size" <> help "OpenCL local work size (default 256)" <> value 256)

pWorkSetSize :: Parser Integer
pWorkSetSize = option readOption 
    (short 'w' <> long "workset-size" <> help "OpenCL workset size (default 64)" <> value 64)

readOption :: Read a => ReadM a
readOption = eitherReader $ \s -> 
  maybe (Left "Invalid argument supplied") Right $ (readMaybe s :: Read a => Maybe a)

pDevice :: Parser GPUDevice
pDevice = option parseDevice (
  short 'd' <> long "device" <> help "Devices are specified in the format 'x,y' where x is the platform ID, and y is the device ID. If no devices are specified, all GPUs will be used.")
 where
  parseDevice :: ReadM GPUDevice
  parseDevice = eitherReader $ \s -> do 
    tup <- case splitOn "," s of 
      [l, r] -> Right (l,r)
      _ -> Left "Device format should be x,y"
    readDevice tup

  readDevice :: (String, String) -> Either String GPUDevice
  readDevice (evilP, evilD) = do
    pStr <- maybe (Left "Platform must be a non-negative integer") Right $ readIndex evilP
    dStr <- maybe (Left "Device must be a non-negative integer") Right $ readIndex evilD
    pure $ GPUDevice pStr dStr

  readIndex :: String -> Maybe Int
  readIndex txt = find (>=0) (readMaybe txt :: Maybe Int)

gpuOpts :: Parser Command
gpuOpts = GPU <$> pGpuEnv <*> pClientArgs

pLog :: Parser LogLevel
pLog = option (eitherReader l)
    (long "log-level" <> metavar "debug|info|warn|error" <> value LevelInfo
    <> help "The minimum level of log messages to display (default: info)")
  where
    l :: String -> Either String LogLevel
    l "debug" = Right LevelDebug
    l "info"  = Right LevelInfo
    l "warn"  = Right LevelWarn
    l "error" = Right LevelError
    l _       = Left "Must be one of debug|info|warn|error"

pUrl :: Parser HostAddress
pUrl = option parseAddress (
  short 'n' <> long "node" <> help "Devices are specified in the format 'x,y' where x is the platform ID, and y is the device ID. If no devices are specified, all GPUs will be used.")
 where
  parseAddress :: ReadM HostAddress
  parseAddress = eitherReader $ \s -> do 
    (host, evilPort) <- case splitOn ":" s of 
      [l, r] -> Right (l, r)
      _ -> Left "Host format should be <HOST:PORT>"
    port <- readPort evilPort
    pure $ HostAddress (T.pack host) port

  readPort :: String -> Either String Int
  readPort p = maybe (Left "Port must be a number between 0 and 65535") Right $ validatePort p

  validatePort :: String -> Maybe Int
  validatePort txt = find (\n -> n>=0 && n <=65535) (readMaybe txt :: Maybe Int)


pMiner :: Parser Miner
pMiner = Miner <$> accountName <*> many key <*> predicate
 where
  accountName :: Parser Text
  accountName = strOption (long "miner-account" <> help "Coin Contract account name of Miner")

  predicate :: Parser Text
  predicate = strOption (long "predicate" <> help "Key set predicate. Default should be fine" <> value "keys-all")
  
  key :: Parser Text 
  key = option mKey (long "miner-key" <> help "(Optional) The public key of the account you want to mine to.  If this is not set, the value supplied for --miner-account will be used.")

  mKey :: ReadM Text
  mKey = eitherReader $ \s -> do
      unless (validatePKey s)
          . Left $ "Public Key " <> s <> " is not valid."
      Right $ T.pack s

validatePKey :: String -> Bool
validatePKey s = length s == 64 && all isHexDigit s

donateTo :: Miner
donateTo = Miner "88a56a0b99d6cd89a041bae00b58a10832453143c924cd00d3a83e1dc076ee0c" ["88a56a0b99d6cd89a041bae00b58a10832453143c924cd00d3a83e1dc076ee0c"] "keys-all"

data OtherCommand = ShowDevices

showDevicesOpts :: Parser OtherCommand
showDevicesOpts = pure ShowDevices
 
showT :: Show a => a -> Text
showT = T.pack . show

round2 :: RealFrac a => a -> a
round2 = roundN (2 :: Int)

roundN :: (RealFrac a, Integral b) => b -> a -> a
roundN p a = 
  let m = 10 ^ p 
   in fromIntegral (round (a * m) :: Integer) / m

tlsSettings :: TLSSettings
tlsSettings = TLSSettingsSimple True True True
