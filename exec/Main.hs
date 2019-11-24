{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

-- |
-- Module: Main
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Colin Woodbury <colin@kadena.io>
-- Stability: experimental
--
-- The fast "inner loop" of the mining process. Asks for work from a Chainweb
-- Node, and submits results back to it.
--
-- == Purpose and Expectations ==
--
-- This tool is a low-level, pull-based, independent, focusable, multicore CPU
-- and GPU miner for Chainweb. By this we mean:
--
--   * low-level: The miner is not aware of how `BlockHeader`s are encoded into
--     `ByteString`s, as indicated by an external spec. It does not know how to
--     construct a full `BlockHeader` type, nor does it need to. It has no
--     concept of `Cut`s or the cut network used by Chainweb - it simply
--     attempts `Nonce`s until a suitable hash is found that matches the current
--     `HashTarget`.
--
--   * pull-based: Work is requested from some remote Chainweb Node, as
--     configured on the command line. The Miner is given an encoded candidate
--     `BlockHeader`, which it then injects a `Nonce` into, hashes, and checks
--     for a solution. If/when a solution is found, it is sent back to the
--     Chainweb Node to be reassociated with its Payload and published as a new
--     `Cut` to the network.
--
--   * independent: It is assumed that in general, individuals running Chainweb
--     Miners and Chainweb Nodes are separate entities. A Miner requests work
--     from a Node and trusts them to assemble a block. Nodes do not know who is
--     requesting work, but Miners know who they're requesting work from. In
--     this way, there is a many-to-one relationship between Mining Clients and
--     a Node.
--
--   * focusable: A Miner can be configured to prioritize work belonging to a
--     specific chain. Note, however, that if a work request for a particular
--     chain can't be filled by a Node (if say that Chain has progressed too far
--     relative to its neighbours), then the Node will send back whatever it
--     could find. This strategy is to balance the needs of Miners who have a
--     special interest in progressing a specific chain with the needs of the
--     network which requires even progress across all chains.
--
--   * multicore: The miner uses 1 CPU core by default, but can use as many as
--     you indicate. GPU support is also available.
--

module Main ( main ) where

import           Miner.Kernel
import qualified Control.Monad as M
import           Control.Retry
import           Data.Aeson (toJSON)
import           Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Data.List as L
import           Data.Tuple.Strict (T2(..), T3(..))
import           Options.Applicative
import           RIO
import qualified RIO.ByteString as B
import qualified RIO.ByteString.Lazy as BL
import           RIO.List.Partial ((!!))
import qualified RIO.NonEmpty as NEL
import qualified RIO.NonEmpty.Partial as NEL
import qualified RIO.Text as T
import           Network.HTTP.Client.TLS (mkManagerSettings)
import           Network.HTTP.Client (newManager)
import qualified System.Random.MWC as MWC
import           Text.Printf (printf)

-- internal modules
import           Miner.OpenCL
import           Miner.Types
import           Miner.Chainweb
import           Miner.Updates
import           Miner.Http
import qualified Text.PrettyPrint.ANSI.Leijen as PP

--------------------------------------------------------------------------------
-- Work

main :: IO ()
main = execParser opts >>= \case
    GPU env cargs -> work env cargs >> exitFailure
    Otherwise ShowDevices -> PP.putDoc =<< PP.prettyList <$> queryAllOpenCLDevices
  where
    opts :: ParserInfo Command
    opts = info (pCommand <**> helper)
        (fullDesc <> progDesc "The Official Chainweb Mining Client")

work :: GPUEnv -> ClientArgs -> IO ()
work cmd cargs = do
    lopts <- setLogMinLevel (ll cargs) . setLogUseLoc False <$> logOptionsHandle stderr True
    withLogFunc lopts $ \logFunc -> do
        g <- MWC.createSystemRandom
        m <- newManager (mkManagerSettings tlsSettings Nothing)
        euvs <- sequence <$> traverse nodeVer (coordinators cargs)
        case euvs of
            Left e -> throwString $ show e
            Right results -> do
                mUrls <- newIORef $ NEL.fromList results
                totalDevices <- numDevices (gpuDevices cmd)
                stats <- M.replicateM totalDevices (newIORef 0)
                start <- newIORef 0
                successStart <- getPOSIXTime >>= newIORef
                runRIO (Env g m logFunc cmd cargs stats start successStart mUrls) $ do 
                  goodMiner <- validateMiner (miner cargs)
                  runRIO (Env g m logFunc cmd (ClientArgs (ll cargs) (coordinators cargs) goodMiner) stats start successStart mUrls) run
  where
    nodeVer :: HostAddress -> IO (Either Text (T2 HostAddress Text))
    nodeVer baseurl = (T2 baseurl <$>) <$> getInfo baseurl

    numDevices [] = fmap (\ds -> length $ platformDevices =<< ds) queryAllOpenCLDevices
    numDevices ds = pure $ length ds
    
    validateMiner :: Miner -> RIO Env Miner
    validateMiner (Miner acct [] pred) =
      if not $ validatePKey (T.unpack acct)
      then 
        logWarn "No mining key supplied, and account name is not a valid public key.  Either specify a public key, or use an account-name which is the same as your public key." >>
          liftIO (exitWith (ExitFailure 1))
      else pure $ Miner acct [acct] pred
    validateMiner m = pure m



getInfo :: HostAddress -> IO (Either Text Text)
getInfo url = fmap nodeVersion <$> (getJSON url "/info" :: (IO (Either Text NodeInfo)))

run :: RIO Env ()
run = do
  logInfo "Starting Miner."
  s <- scheme
  mining s


scheme :: RIO Env (TargetBytes -> HeaderBytes -> RIO Env HeaderBytes)
scheme = do
  env <- ask 
  opencl <$> setUpOpenCL (envGpu env)

setUpOpenCL :: GPUEnv -> RIO Env [OpenCLWork]
setUpOpenCL oce = do
    platforms <- liftIO queryAllOpenCLDevices
    devices <- fetchDevices platforms $ gpuDevices oce
    logInfo $ "Beginning mining on " <> display (length devices) <> " GPUs."
    traverse_ (logInfo . display) (T.pack . (\(idx, d) -> "GPU #" <> show idx <> ": " <> T.unpack (deviceName d) <> " @ " <> (show $ deviceMaxClock d) <> " MHz") <$> zip (L.findIndices (const True) devices) devices)
    
    liftIO $ traverse (\d -> prepareOpenCLWork kernel d [wss] "search_nonce") devices
 where
  wss :: Text
  wss = T.pack $ "-DWORKSET_SIZE=" <> show (workSetSize oce)

  fetchDevices :: [OpenCLPlatform] -> [GPUDevice] -> RIO Env [OpenCLDevice]
  fetchDevices plats [] = pure $ plats >>= platformDevices
  fetchDevices plats devs = traverse (validateDevice plats) devs

  validateDevice :: [OpenCLPlatform] -> GPUDevice -> RIO Env OpenCLDevice
  validateDevice platforms dev = do
    let pIndex = platformIndex dev
    let platformCount = length platforms
    when (pIndex >= platformCount) $ do
        logError . display . T.pack $
            "Selected platform index " <> show pIndex
         <> " but there are only " <> show platforms <> " of them."
        exitFailure

    let platform = platforms !! pIndex

    let devices = platformDevices platform
    let dIndex = deviceIndex dev
    let deviceCount = length devices
    when (deviceIndex dev >= deviceCount) $ do
        logError . display . T.pack $
            "Selected device index " <> show (deviceIndex dev) 
         <> " but there are only " <> show (length devices) <> " of them on the selected platform."
        exitFailure

    pure $ devices !! dIndex

-- | Attempt to get new work while obeying a sane retry policy.
--
getWork :: RIO Env (Maybe WorkBytes)
getWork = do
    logDebug "Attempting to fetch new work from the remote Node"
    e <- ask
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

    warn :: Either Text WorkBytes -> RIO Env Bool
    warn (Right _) = pure False
    warn (Left se) = logWarn (display se) $> True

    f :: Env -> IO (Either Text WorkBytes)
    f e = do
        T2 h v <- NEL.head <$> readIORef (envUrls e)
        acct <- runRIO e getMiningAcct
        response <- getWithBody h ("/chainweb/0.0/" <> T.unpack v <> "/mining/work") (toJSON acct)
        pure (WorkBytes . BL.toStrict <$> response)
      where
        getMiningAcct = do
          env <- ask
          seconds :: Integer <- liftIO $ round <$> getPOSIXTime 
          let donateRate = 10
          let minutes :: Integer = round $ (fromIntegral seconds :: Double) / 60
          let donateTimeRemaining = mod minutes 100 - (100 - donateRate)
          let donate = (donateTimeRemaining > 0)
          when donate (logInfo $ display ("Donating time for " <> showT donateTimeRemaining <> " minutes"))
          pure $ if donate then donateTo else (miner . envArgs) env

-- -------------------------------------------------------------------------- --
-- Mining 

workKey :: WorkBytes -> RIO Env UpdateKey
workKey wb = do
    e <- ask
    T2 url _ <- NEL.head <$> readIORef (envUrls e)
    cid <- liftIO $ chain cbytes
    return $! UpdateKey
        { _updateKeyChainId = cid
        , _updateKeyHost = T.unpack $ addressHostname url
        , _updateKeyPort = addressPort url
        }
  where
    T3 cbytes _ _ = unWorkBytes wb

-- | A supervisor thread that listens for new work and manages mining threads.
--
-- TODO: use exponential backoff instead of fixed delay when retrying.
-- (restart retry sequence when the maximum retry count it reached)
--
mining :: (TargetBytes -> HeaderBytes -> RIO Env HeaderBytes) -> RIO Env ()
mining go = do
    updateMap <- newUpdateMap -- TODO use a bracket
    miningLoop updateMap go `finally` logInfo "Miner halted."

data Recovery = Irrecoverable | Recoverable

-- | The inner mining loop.
--
-- It uses 'getWork' to obtain new work and starts mining. If a block is solved it
-- calls 'miningSuccess' and loops around.
--
-- It also starts over with new work when an update is triggered.
--
-- NOTE: if this fails continuously, e.g. because of a miss-configured or buggy
-- miner, this function will spin forever!
--
-- TODO: add rate limiting for failures and raise an error if the function fails
-- at an higher rate.
--
miningLoop ::  UpdateMap -> (TargetBytes -> HeaderBytes -> RIO Env HeaderBytes) -> RIO Env ()
miningLoop updateMap inner = mask go
  where
    logHashRates :: Double -> RIO Env ()
    logHashRates seconds = do
      e <- ask
      let nonceStepSize = (globalSize . envGpu $ e) * (workSetSize . envGpu $ e)
      let ioRefs = envHashes e
      stepTotals <- liftIO $ traverse readIORef ioRefs
      let hashTotals = (* nonceStepSize) . fromIntegral <$> stepTotals
      traverse_ (\(idx, hash) -> logHashRate idx seconds hash) (zip (L.findIndices (const True) hashTotals) hashTotals)

    logHashRate :: Int -> Double -> Integer -> RIO Env ()
    logHashRate idx secs hashes = 
      let rate = fromIntegral hashes / max 1 secs / 1000000
       in when (rate > 0) $ logInfo . display . T.pack $
              printf "GPU #%d: %.2f MH/s" idx rate

    go :: (RIO Env () -> RIO Env a) -> RIO Env ()
    go umask = (forever (umask loopBody) `catches` handlers) >>= \case
        Irrecoverable -> pure ()
        Recoverable   -> threadDelay 1_000_000 >> go umask
      where
        handlers =
            [ Handler $ \(e :: IOException) -> logError (display e) >> pure Irrecoverable
            , Handler $ \(e :: SomeException) -> do
                logWarn "Some general error in mining loop. Trying again..."
                logDebug $ display e
                pure Recoverable
            ]

    loopBody :: RIO Env ()
    loopBody = do
        startTime <- liftIO getPOSIXTime
        w <- getWork >>= \case
            Nothing -> exitFailure
            Just x -> return x
        let T3 cbytes tbytes hbytes = unWorkBytes w
        cid <- liftIO $ chainInt cbytes
        updateKey <- workKey w
        logDebug . display . T.pack $ printf "Chain %d: Start mining on new work item." cid
        res <- withPreemption updateMap updateKey (inner tbytes hbytes) 
        e <- ask
        endTime <- liftIO getPOSIXTime
        let addSecs  = fromIntegral (round ((endTime - startTime) * 1000000)) `div` 1000000
        modifyIORef' (envSecs e) (+addSecs)
        secs <- readIORef (envSecs e)
        logHashRates $ fromIntegral secs
        handleResult w res
      where
        handleResult w (Right a) = miningSuccess w a
        handleResult _ (Left ()) = logDebug "Mining loop was preempted. Getting updated work ..."

        -- | If the `go` call won the `race`, this function yields the result back
        -- to some "mining coordinator" (likely a chainweb-node).
        --
        miningSuccess :: WorkBytes -> HeaderBytes -> RIO Env ()
        miningSuccess w h = do
            e <- ask
            before <- readIORef (envLastSuccess e)
            now <- liftIO getPOSIXTime
            writeIORef (envLastSuccess e) now
            let !d = ceiling (now - before) :: Int
            cid <- liftIO $ chainInt cbytes
            hgh <- liftIO $ headerHeight hbytes
            logInfo . display . T.pack $ printf "Chain %d: Mined block at Height %d. (%ds since last)" cid hgh d
            T2 host v <- NEL.head <$> readIORef (envUrls e)
            res <- liftIO $ post host ("/chainweb/0.0/" <> T.unpack v <> "/mining/solved") (_headerBytes h)
            when (isLeft res) $ logWarn "Failed to submit new BlockHeader!"
          where
            T3 cbytes _ hbytes = unWorkBytes w

opencl :: [OpenCLWork] -> TargetBytes -> HeaderBytes -> RIO Env HeaderBytes
opencl !works t h@(HeaderBytes blockbytes) = loop 
 where
  loop :: RIO Env HeaderBytes
  loop = do
      e <- ask
      res <- try $ liftIO $ openCLMiner e works t h
      case res of
          Left (err :: SomeException) -> do
              logError . display . T.pack $ "Error running OpenCL miner: " <> show err
              throwM err
          Right nonceBytes -> pure . HeaderBytes $ nonceBytes <> B.drop 8 blockbytes

-- -------------------------------------------------------------------------- --
-- Utils

chain :: MonadThrow m => MonadIO m => ChainBytes -> m ChainId
chain (ChainBytes cbs) = runGet decodeChainId cbs

chainInt :: MonadThrow m => MonadIO m => ChainBytes -> m Int
chainInt c = chainIdInt <$> chain c

headerHeight :: MonadThrow m => MonadIO m => HeaderBytes -> m Word64
headerHeight (HeaderBytes hbs) = height <$> runGet decodeBlockHeight (B.take 8 $ B.drop 258 hbs)
