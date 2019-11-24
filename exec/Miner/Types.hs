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
  , Magnitude(..)
  , HeaderBytes(..)
  , TargetBytes(..)
  , ChainBytes(..)
  , WorkBytes(..)
  , HostAddress(..)
  , ChainId
    -- * miscellaneous
  , tlsSettings
  , donateTo
  , reduceMag
  , showT
  , encodeChainId
  ) where

import           Data.Generics.Product.Fields (field)
import           Data.List
import           Data.Bytes.Put
import           Data.Bytes.Signed (unsigned)
import qualified Data.ByteString.Char8 as B
import           Data.List.Split
import qualified Data.Text as T
import           Data.Time.Clock.POSIX (POSIXTime)
import           Data.Tuple.Strict (T2(..))
import           Network.Connection
import           Network.HTTP.Client hiding (Proxy(..), responseBody, port, host)
import           Options.Applicative
import           RIO as R
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
  { accountName :: Text
  , publicKeys :: [Text]
  , predicate :: Text
  }

-- | The top-level git-style CLI "command" which determines which mining
-- paradigm to follow.
--
data Command = GPU GPUEnv ClientArgs | Otherwise OtherCommand

data GPUEnv = GPUEnv 
    { gpuDevices :: [GPUDevice] 
    , globalSize :: Int
    , localSize :: Int
    , workSetSize :: Int
    } deriving stock (Generic)

data GPUDevice = GPUDevice
  { platformIndex :: Int
  , deviceIndex :: Int
  } deriving stock (Generic)

newtype HeaderBytes = HeaderBytes B.ByteString
newtype TargetBytes = TargetBytes B.ByteString
newtype ChainBytes = ChainBytes B.ByteString
newtype WorkBytes = WorkBytes B.ByteString

type ChainId = Word32

encodeChainId :: MonadPut m => ChainId -> m ()
encodeChainId i32 = putWord32le $ unsigned i32
{-# INLINE encodeChainId #-}


pClientArgs :: Parser ClientArgs
pClientArgs = ClientArgs <$> pLog <*> some pUrl <*> pMiner 

pCommand :: Parser Command
pCommand = hsubparser
    (  command "mine" (info gpuOpts (progDesc "Perform mining"))
    <> command "keys" (info (Otherwise <$> keysOpts) (progDesc "Generate public/private key pair"))
    <> command "show-devices" (info (Otherwise <$> showDevicesOpts) (progDesc "Show all available OpenCL Devices"))
    )

pGpuEnv :: Parser GPUEnv
pGpuEnv = GPUEnv <$> many pDevice <*> pGlobalSize <*> pLocalSize <*> pWorkSetSize  

pGlobalSize :: Parser Int
pGlobalSize = option readOption
    (short 'g' <> long "global-size" <> help "OpenCL global work size (default 1024*1024*16)" <> value (1024*1024*16))

pLocalSize :: Parser Int
pLocalSize = option readOption
    (short 'l' <> long "local-size" <> help "OpenCL local work size (default 256)" <> value 256)

pWorkSetSize :: Parser Int
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
  key = option mKey (long "miner-key" <> help "The public key of the account you want to mine to.")

  mKey :: ReadM Text
  mKey = eitherReader $ \s -> do
      unless (length s == 64 && all isHexDigit s)
          . Left $ "Public Key " <> s <> " is not valid."
      Right $ T.pack s

donateTo :: Miner
donateTo = Miner "JayKobe6k" ["84811e7773ec9f6546d8baaf48c79119414b4bed3bfe752c82af6326e5d6b7ff"] "keys-all"

data OtherCommand = ShowDevices | Keys 

keysOpts :: Parser OtherCommand
keysOpts = pure Keys

showDevicesOpts :: Parser OtherCommand
showDevicesOpts = pure ShowDevices

data Magnitude = B | K | M | G deriving (Eq, Show)
   
showT :: Show a => a -> Text
showT = T.pack . show

reduceMag :: Magnitude -> Magnitude -> Double -> Text
reduceMag B mx a = if a > 1024 && mx /= B then reduceMag K mx (a / 1024) else (showT . round2) a <> " "
reduceMag K mx a = if a > 1024 && mx /= K then reduceMag M mx (a / 1024) else (showT . round2) a <> " K"
reduceMag M mx a = if a > 1024 && mx /= M then reduceMag G mx (a / 1024) else (showT . round2) a <> " M"
reduceMag G _ a = (showT . round2) a <> " G"

round2 :: Double -> Double
round2 a = fromIntegral (round (a * 100) :: Integer) / 100

tlsSettings :: TLSSettings
tlsSettings = TLSSettingsSimple True True True
