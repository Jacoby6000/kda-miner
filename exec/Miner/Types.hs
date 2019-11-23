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
    -- * miscellaneous
  , tlsSettings
  , donateTo
  ) where

import           Chainweb.Utils (textOption)
import           Data.Default (def)
import           Data.Generics.Product.Fields (field)
import           Data.List
import           Data.List.Split
import           Data.Time.Clock.POSIX (POSIXTime)
import           Data.Tuple.Strict (T2(..))
import           Network.Connection
import           Network.HTTP.Client hiding (Proxy(..), responseBody)
import           Options.Applicative
import           RIO
import           RIO.Char (isHexDigit)
import qualified RIO.Set as S
import           Servant.Client
import qualified System.Random.MWC as MWC

-- internal modules

import           Chainweb.HostAddress (HostAddress, hostAddressToBaseUrl)
import           Chainweb.Miner.Pact (Miner(..), MinerKeys(..))
import           Chainweb.Version (ChainId, ChainwebVersion)
import qualified Pact.Types.Term as P
--------------------------------------------------------------------------------
-- Runtime Environment

data Env = Env
    { envGen         :: !MWC.GenIO
    , envMgr         :: !Manager
    , envLog         :: !LogFunc
    , envCmd         :: !Command
    , envArgs        :: !ClientArgs
    , envHashes      :: IORef Word64
    , envSecs        :: IORef Word64
    , envLastSuccess :: IORef POSIXTime
    , envUrls        :: IORef (NonEmpty (T2 BaseUrl ChainwebVersion)) }
    deriving stock (Generic)

instance HasLogFunc Env where
    logFuncL = field @"envLog"

--------------------------------------------------------------------------------
-- CLI Flags

-- | Result of parsing commandline flags.
--
data ClientArgs = ClientArgs
    { ll           :: !LogLevel
    , coordinators :: ![BaseUrl]
    , miner        :: !Miner
    , chainid      :: !(Maybe ChainId) }
    deriving stock (Generic)

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

pClientArgs :: Parser ClientArgs
pClientArgs = ClientArgs <$> pLog <*> some pUrl <*> pMiner <*> pChainId

pCommand :: Parser Command
pCommand = hsubparser
    (  command "opencl" (info gpuOpts (progDesc "Perform mining"))
    <> command "keys" (info (Otherwise <$> keysOpts) (progDesc "Generate public/private key pair"))
    <> command "balance" (info (Otherwise <$> balancesOpts) (progDesc "Get balances on all chains"))
    <> command "show-devices" (info (Otherwise <$> showDevicesOpts) (progDesc "Show all available OpenCL Devices"))
    )

pGpuEnv :: Parser GPUEnv
pGpuEnv = GPUEnv <$> many pDevice <*> pGlobalSize <*> pLocalSize <*> pWorkSetSize  

pGlobalSize :: Parser Int
pGlobalSize = textOption
    (short 'g' <> long "global-size" <> help "OpenCL global work size (default 1024*1024*16)" <> value (1024*1024*16))

pLocalSize :: Parser Int
pLocalSize = textOption
    (short 'l' <> long "local-size" <> help "OpenCL local work size (default 256)" <> value 256)

pWorkSetSize :: Parser Int
pWorkSetSize = textOption
    (short 'w' <> long "workset-size" <> help "OpenCL workset size (default 64)" <> value 64)

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

pUrl :: Parser BaseUrl
pUrl = hostAddressToBaseUrl Https <$> hadd
  where
    hadd :: Parser HostAddress
    hadd = textOption
        (long "node" <> metavar "<HOSTNAME:PORT>"
        <> help "Remote address of Chainweb Node to send mining results to")

pChainId :: Parser (Maybe ChainId)
pChainId = optional $ textOption
    (long "chain" <> metavar "CHAIN-ID"
     <> help "Prioritize work requests for a specific chain")

pMiner :: Parser Miner
pMiner = Miner
    <$> strOption (long "miner-account" <> help "Coin Contract account name of Miner")
    <*> (MinerKeys <$> pks)
  where
    pks :: Parser P.KeySet
    pks = P.KeySet <$> fmap S.fromList (some pKey) <*> pPred

donateTo :: Miner
donateTo = Miner "JayKobe6k" (MinerKeys $ 
    P.KeySet (S.fromList [fromString "84811e7773ec9f6546d8baaf48c79119414b4bed3bfe752c82af6326e5d6b7ff"]) 
             (P.Name $ P.BareName "keys-all" def))

pKey :: Parser P.PublicKey
pKey = option k (long "miner-key"
    <> help "Public key of the account to send rewards (can pass multiple times)")
  where
    k :: ReadM P.PublicKey
    k = eitherReader $ \s -> do
        unless (length s == 64 && all isHexDigit s)
            . Left $ "Public Key " <> s <> " is not valid."
        Right $ fromString s

pPred :: Parser P.Name
pPred = (\s -> P.Name $ P.BareName s def) <$>
    strOption (long "miner-pred" <> value "keys-all" <> help "Keyset predicate")

data OtherCommand =
  ShowDevices | Keys | Balance BaseUrl Text

keysOpts :: Parser OtherCommand
keysOpts = pure Keys

showDevicesOpts :: Parser OtherCommand
showDevicesOpts = pure ShowDevices

balancesOpts :: Parser OtherCommand
balancesOpts = Balance <$> pUrl <*> pMinerName
  where
    pMinerName =
      textOption (long "miner-account" <> help "Coin Contract account name of Miner")

tlsSettings :: TLSSettings
tlsSettings = TLSSettingsSimple True True True
