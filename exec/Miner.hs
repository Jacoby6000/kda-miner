{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Main ( main ) where

import           Control.Parallel.OpenCL
import           Control.Retry
import           Data.Default (def)
import           Data.Generics.Product.Fields (field)
import           Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import           Data.Tuple.Strict (T2(..), T3(..))
import           Data.Text.IO
import           Network.Connection (TLSSettings(..))
import           Network.HTTP.Client hiding (Proxy(..), responseBody)
import           Network.HTTP.Client.TLS (mkManagerSettings)
import           Network.HTTP.Types.Status (Status(..))
import           Network.Wai.EventSource (ServerEvent(..))
import           Network.Wai.EventSource.Streaming (withEvents)
import           Options.Applicative
import           RIO
import qualified RIO.ByteString as B
import qualified RIO.ByteString.Lazy as BL
import           RIO.Char (isHexDigit)
import qualified RIO.NonEmpty as NEL
import qualified RIO.NonEmpty.Partial as NEL
import qualified RIO.Set as S
import qualified RIO.Text as T
import           Servant.Client
import qualified Streaming.Prelude as SP
import qualified System.Random.MWC as MWC
import           Text.Printf (printf)

-- internal modules

import           Chainweb.BlockHeader
import           Chainweb.BlockHeader.Validation (prop_block_pow)
import           Chainweb.HostAddress (HostAddress, hostAddressToBaseUrl)
import           Chainweb.Miner.Core
import           Chainweb.Miner.Pact (Miner(..), MinerKeys(..))
import           Chainweb.Miner.RestAPI.Client (solvedClient, workClient)
import           Chainweb.RestAPI.NodeInfo (NodeInfo(..), NodeInfoApi)
import           Chainweb.Utils (runGet, textOption, toText)
import           Chainweb.Version

import qualified Pact.Types.Term as P

--------------------------------------------------------------------------------
-- CLI

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
data Command = GPU GPUEnv ClientArgs | Devices

data GPUEnv = GPUEnv
    { envKernelPath :: Text
    , envPlatformId :: Integer
    , envDeviceId   :: Integer 
    } deriving stock (Generic)

data Env = Env
    { envGen         :: !MWC.GenIO
    , envMgr         :: !Manager
    , envLog         :: !LogFunc
    , envGpu         :: !GPUEnv
    , envArgs        :: !ClientArgs
    , envHashes      :: IORef Word64
    , envSecs        :: IORef Word64
    , envLastSuccess :: IORef POSIXTime
    , envUrls        :: IORef (NonEmpty (T2 BaseUrl ChainwebVersion)) }
    deriving stock (Generic)

instance HasLogFunc Env where
    logFuncL = field @"envLog"

pClientArgs :: Parser ClientArgs
pClientArgs = ClientArgs <$> pLog <*> some pUrl <*> pMiner <*> pChainId

pCommand :: Parser Command
pCommand = hsubparser (command "gpu" (info gpuOpts (progDesc "Perform GPU mining")) 
                    <> command "list-devices" (info (pure Devices) (progDesc "List available GPU devices") ))

pKernelPath :: Parser Text
pKernelPath = textOption
    (long "kernel-path" <> help "Path to chainweb-gpu-miner executable")

pGpuEnv :: Parser GPUEnv
pGpuEnv = GPUEnv <$> pKernelPath <*> pure 1 <*> pure 0

gpuOpts :: Parser Command
gpuOpts = liftA2 GPU pGpuEnv pClientArgs

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
    pks = P.KeySet <$> (fmap S.fromList $ some pKey) <*> pPred

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

--------------------------------------------------------------------------------
-- Work

main :: IO ()
main = execParser opts >>= \case
    (GPU gpuEnv clientArgs) -> work gpuEnv clientArgs >> exitFailure
    Devices -> showDevices >>= putStrLn
  where
    opts :: ParserInfo Command
    opts = info (pCommand <**> helper)
        (fullDesc <> progDesc "The Official Chainweb Mining Client")

work :: GPUEnv -> ClientArgs -> IO ()
work cmd cargs = do
    lopts <- setLogMinLevel (ll cargs) . setLogUseLoc False <$> logOptionsHandle stderr True
    withLogFunc lopts $ \logFunc -> do
        g <- MWC.createSystemRandom
        m <- newManager (mkManagerSettings ss Nothing)
        euvs <- sequence <$> traverse (nodeVer m) (coordinators cargs)
        case euvs of
            Left e -> throwString $ show e
            Right results -> do
                mUrls <- newIORef $ NEL.fromList results
                stats <- newIORef 0
                start <- newIORef 0
                successStart <- getPOSIXTime >>= newIORef
                runRIO (Env g m logFunc cmd cargs stats start successStart mUrls) run
  where
    nodeVer :: Manager -> BaseUrl -> IO (Either ClientError (T2 BaseUrl ChainwebVersion))
    nodeVer m baseurl = (T2 baseurl <$>) <$> getInfo m baseurl

    -- | This allows this code to accept the self-signed certificates from
    -- `chainweb-node`.
    --
    ss :: TLSSettings
    ss = TLSSettingsSimple True True True

getInfo :: Manager -> BaseUrl -> IO (Either ClientError ChainwebVersion)
getInfo m url = fmap nodeVersion <$> runClientM (client (Proxy @NodeInfoApi)) cenv
  where
    cenv = ClientEnv m url Nothing

run :: RIO Env ()
run = do
    logInfo "Starting Miner."
    getWork >>= traverse_ mining 

-- | Attempt to get new work while obeying a sane retry policy.
--
getWork :: RIO Env (Maybe WorkBytes)
getWork = do
    e <- ask
    logDebug "Attempting to fetch new work from the remote Node"
    retrying policy (const warn) (const . liftIO $ f e) >>= \case
        Left _ -> do
            logWarn "Failed to fetch work! Switching nodes..."
            urls <- readIORef $ envUrls e
            case NEL.nonEmpty $ NEL.tail urls of
                Nothing   -> logError "No nodes left!" >> pure Nothing
                Just rest -> writeIORef (envUrls e) rest >> getWork
        Right bs -> pure $ Just bs
  where
    -- | If we wait longer than the average block time and still can't get
    -- anything, then there's no point in continuing to wait.
    --
    policy :: RetryPolicy
    policy = exponentialBackoff 500000 <> limitRetries 7

    warn :: Either ClientError WorkBytes -> RIO Env Bool
    warn (Right _) = pure False
    warn (Left se) = bad se $> True

    bad :: ClientError -> RIO Env ()
    bad (ConnectionError _) = logWarn "Could not connect to the Node."
    bad (FailureResponse _ r) = logWarn $ c <> " from Node: " <> m
      where
        c = display . statusCode $ responseStatusCode r
        m = displayBytesUtf8 . BL.toStrict $ responseBody r
    bad _ = logError "Something truly bad has happened."

    f :: Env -> IO (Either ClientError WorkBytes)
    f e = do
        T2 u v <- NEL.head <$> readIORef (envUrls e)
        runClientM (workClient v (chainid a) $ miner a) (ClientEnv m u Nothing)
      where
        a = envArgs e
        m = envMgr e

-- | A supervisor thread that listens for new work and manages mining threads.
--
mining :: WorkBytes -> RIO Env ()
mining wb = do
    e <- ask 
    clProgram <- loadProgram $ envGpu e
    race updateSignal (runMiner (envGpu e) tbytes hbytes) >>= traverse_ miningSuccess
    getWork >>= traverse_ mining
  where
    T3 (ChainBytes cbs) tbytes hbytes@(HeaderBytes hbs) = unWorkBytes wb


    loadProgram :: GPUEnv -> CLProgram
    loadProgram env = do
      contents <- readFile $ T.unpack (envKernelPath env)
      clCreateProgramWithSource ctx $ T.unpack contents

    chain :: IO Int
    chain = chainIdInt <$> runGet decodeChainId cbs

    height :: IO Word64
    height = _height <$> runGet decodeBlockHeight (B.take 8 $ B.drop 258 hbs)

    -- TODO Rework to use Servant's streaming? Otherwise I can't use the
    -- convenient client function here.
    updateSignal :: RIO Env ()
    updateSignal = catchAny f $ \se -> do
        logWarn "Couldn't connect to update stream. Trying again..."
        logDebug $ display se
        threadDelay 1000000  -- One second
        updateSignal
      where
        f :: RIO Env ()
        f = do
            e <- ask
            u <- NEL.head <$> readIORef (envUrls e)
            liftIO $ withEvents (req u) (envMgr e) (void . SP.head_ . SP.filter realEvent)
            cid <- liftIO chain
            logDebug . display . T.pack $ printf "Chain %d: Current work was preempted." cid

        -- TODO Formalize the signal content a bit more?
        realEvent :: ServerEvent -> Bool
        realEvent ServerEvent{} = True
        realEvent _             = False

        -- TODO This is an uncomfortable URL hardcoding.
        req :: T2 BaseUrl ChainwebVersion -> Request
        req (T2 u v) = defaultRequest
            { host = encodeUtf8 . T.pack . baseUrlHost $ u
            , path = "chainweb/0.0/" <> encodeUtf8 (toText v) <> "/mining/updates"
            , port = baseUrlPort u
            , secure = True
            , method = "GET"
            , requestBody = RequestBodyBS cbs
            , responseTimeout = responseTimeoutNone }

    -- | If the `go` call won the `race`, this function yields the result back
    -- to some "mining coordinator" (likely a chainweb-node). If `updateSignal`
    -- won the race instead, then the `go` call is automatically cancelled.
    --
    miningSuccess :: HeaderBytes -> RIO Env ()
    miningSuccess h = do
      e <- ask
      secs <- readIORef (envSecs e)
      hashes <- readIORef (envHashes e)
      before <- readIORef (envLastSuccess e)
      now <- liftIO getPOSIXTime
      writeIORef (envLastSuccess e) now
      let !m = envMgr e
          !r = (fromIntegral hashes :: Double) / max 1 (fromIntegral secs) / 1000000
          !d = ceiling (now - before) :: Int
      cid <- liftIO chain
      hgh <- liftIO height
      logInfo . display . T.pack $
          printf "Chain %d: Mined block at Height %d. (%.2f MH/s - %ds since last)" cid hgh r d
      T2 url v <- NEL.head <$> readIORef (envUrls e)
      res <- liftIO . runClientM (solvedClient v h) $ ClientEnv m url Nothing
      when (isLeft res) $ logWarn "Failed to submit new BlockHeader!"

runMiner :: GPUEnv -> TargetBytes -> HeaderBytes -> RIO Env HeaderBytes
runMiner t h@(HeaderBytes blockbytes) = do
    e <- ask
    res <- liftIO $ runGPU  t h
    case res of
      Left err -> do
          logError . display . T.pack $ "Error running GPU miner: " <> err
          throwString err
      Right (MiningResult nonceBytes numNonces hps _) -> do
          let newBytes = nonceBytes <> B.drop 8 blockbytes
              secs = numNonces `div` max 1 hps

          -- FIXME Consider removing this check if during later benchmarking it
          -- proves to be an issue.
          bh <- runGet decodeBlockHeaderWithoutHash newBytes

          if | not (prop_block_pow bh) -> do
                 logError "Bad nonce returned from GPU!"
                 runMiner t h
             | otherwise -> do
                 modifyIORef' (envHashes e) (+ numNonces)
                 modifyIORef' (envSecs e) (+ secs)
                 pure $! HeaderBytes newBytes

runGPU :: CLContext -> CLProgram -> TargetBytes -> HeaderBytes -> IO (Either String MiningResult)
runGPU ctx prg (TargetBytes target) (HeaderBytes head) = 

showDevices :: IO Text
showDevices = pure "Nothing, yet"
