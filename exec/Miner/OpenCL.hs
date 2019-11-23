 
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ScopedTypeVariables     #-}

module Miner.OpenCL(
  queryAllOpenCLDevices,
  openCLMiner,
  filterDevices,
  findDevice,
  prepareOpenCLWork,
  releaseWork,
  OpenCLPlatform(..),
  OpenCLDevice(..),
  OpenCLWork(..)
) where

import           Control.Exception.Safe
import           Control.Parallel.OpenCL
import           Control.Concurrent.Async
import           Data.ByteString(ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as BSL
import           Data.Foldable
import           Data.Text(Text)
import qualified Data.Text as T
import           Data.Time(diffUTCTime, getCurrentTime)
import           Data.Tuple.Strict(T2(..))
import qualified Data.List as L
import           Data.Word
import           Foreign.C.Types
import           Foreign.Storable
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Array
import           Foreign.Ptr
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import           System.Random

import           Chainweb.Miner.Core

import           Miner.Types(GPUEnv(..))

data OpenCLPlatform = OpenCLPlatform
  { platformId :: !CLPlatformID
  , platformName :: !Text
  , platformVersion :: !Text
  , platformVendor :: !Text
  , platformExtensions :: ![Text]
  , platformDevices :: ![OpenCLDevice]
  } deriving Show

data OpenCLDevice = OpenCLDevice
  { deviceId :: !CLDeviceID
  , devicePlatformId :: !CLPlatformID
  , deviceName :: !Text
  , deviceVendor :: !Text
  , deviceVersion :: !Text
  , deviceTypes :: ![CLDeviceType]
  , deviceAvailable :: !Bool
  , deviceExtensions :: ![Text]
  , deviceAddressBits :: !CLuint
  , deviceGlobalMemSize :: !CLulong
  , deviceGlobalCacheSize :: !CLulong
  , deviceCacheLineSize :: !CLuint
  , deviceLocalMemSize :: !CLulong
  , deviceMaxClock :: !CLuint
  , deviceComputeUnits :: !CLuint
  , deviceMaxWorkGroupSize :: !CSize
  , deviceMaxWorkItemSizes :: ![CSize]
  } deriving Show

data OpenCLWork = OpenCLWork
  { workContext :: !CLContext
  , workQueues :: ![OpenCLWorkQueue]
  , workSource :: !Text
  , workProgram :: !CLProgram
  , workKernel :: !CLKernel
  , workPrepareInputBuf :: Int -> Ptr () -> IO CLMem
  , workOutputBuf :: !CLMem
  } 

data OpenCLWorkQueue = OpenCLWorkQueue
  { workDevice :: !OpenCLDevice
  , workQueue :: !CLCommandQueue
  } deriving Show

instance PP.Pretty OpenCLPlatform where
  prettyList = pList "Platform"
  pretty (OpenCLPlatform _ _ version _ _ devices) =
    text version <> PP.hardline <> PP.prettyList devices <> PP.hardline

instance PP.Pretty OpenCLDevice where
  prettyList = pList "Device"
  pretty (OpenCLDevice _ _ name _ _ tpe ava _ addressBits globalMemSize globalCacheSize _ localMemSize maxClock computeUnits workGroupSize itemSize) =
    text name <> PP.hardline <>
      PP.vsep [
        text "Max Clock Speed:     " <> integralDoc maxClock <> text " MHz",
        text "Total Memory:        " <> docBytes globalMemSize,
        text "Local Memory:        " <> docBytes localMemSize,
        text "Total Cache:         " <> docBytes globalCacheSize,
        text "Compute Units:       " <> integralDoc computeUnits,
        text "Max Work Group Size: " <> integralDoc workGroupSize,
        text "Item sizes:          " <> PP.encloseSep PP.empty PP.empty PP.comma (integralDoc <$> itemSize),
        text "Address Space:       " <> integralDoc addressBits <> text " Bits",
        text "Type:                " <> PP.text (show tpe),
        text "Status:              " <> available ava
      ] <> PP.hardline
   where
     available :: Bool -> PP.Doc
     available True = PP.green $ text "Available"
     available False = PP.red $ text "Unavailable"

buildOpenCLPlatform :: CLPlatformID -> IO OpenCLPlatform
buildOpenCLPlatform pId = do
    name <- platformInfo CL_PLATFORM_NAME
    version <- platformInfo CL_PLATFORM_VERSION
    vendor <- platformInfo CL_PLATFORM_VENDOR
    extensions <- T.splitOn " " <$> platformInfo CL_PLATFORM_EXTENSIONS
    deviceIds <- clGetDeviceIDs pId CL_DEVICE_TYPE_ALL
    devices <- traverse (buildDevice pId) deviceIds
    pure $ OpenCLPlatform pId name version vendor extensions devices
    where
    platformInfo prop = T.pack <$> clGetPlatformInfo pId prop

buildDevice :: CLPlatformID -> CLDeviceID -> IO OpenCLDevice
buildDevice pId dId = do
  name <- T.pack <$> clGetDeviceName dId
  version <- T.pack <$> clGetDeviceVersion dId
  vendor <- T.pack <$> clGetDeviceVendor dId
  tpe <- clGetDeviceType dId
  available <- clGetDeviceAvailable dId
  extensions <- T.splitOn " " . T.pack <$> clGetDeviceExtensions dId
  addressBits <- clGetDeviceAddressBits dId
  globalMemSize <- clGetDeviceGlobalMemSize dId
  globalCacheSize <- clGetDeviceGlobalMemCacheSize dId
  cacheLineSize <- clGetDeviceGlobalMemCachelineSize dId
  localMemSize <- clGetDeviceLocalMemSize dId
  maxClock <- clGetDeviceMaxClockFrequency dId
  computeUnits <- clGetDeviceMaxComputeUnits dId
  workGroupSize <- clGetDeviceMaxWorkGroupSize dId
  itemSize <- clGetDeviceMaxWorkItemSizes dId
  pure $ OpenCLDevice dId pId name vendor version tpe available extensions addressBits globalMemSize globalCacheSize cacheLineSize localMemSize maxClock computeUnits workGroupSize itemSize

queryAllOpenCLDevices :: IO [OpenCLPlatform]
queryAllOpenCLDevices = do
  platformIds <- clGetPlatformIDs
  traverse buildOpenCLPlatform platformIds

releaseWork :: OpenCLWork -> IO Bool
releaseWork w = do
  obj <- clReleaseMemObject $ workOutputBuf w
  kern <- clReleaseKernel $ workKernel w
  prog <- clReleaseProgram $ workProgram w
  queues <- traverse clReleaseCommandQueue (workQueue <$> workQueues w)
  ctx <- clReleaseContext (workContext w)
  pure (obj && kern && prog && ctx && and queues)

text :: Text -> PP.Doc
text = PP.text <$> T.unpack

data ByteMagnitude = B | K | M | G

docBytes :: Integral a => a -> PP.Doc
docBytes n = reduceBytes (fromIntegral n :: Double) B
  where
    reduceBytes :: Double -> ByteMagnitude -> PP.Doc
    reduceBytes a B = if a > 1024 then reduceBytes (a / 1024) K else (PP.double . round2) a <> text " B"
    reduceBytes a K = if a > 1024 then reduceBytes (a / 1024) M else (PP.double . round2) a <> text " KB"
    reduceBytes a M = if a > 1024 then reduceBytes (a / 1024) G else (PP.double . round2) a <> text " MB"
    reduceBytes a G = (PP.double . round2) a <> text " GB"

    round2 :: Double -> Double
    round2 a = fromIntegral (round (a * 100) :: Integer) / 100

integralDoc :: (Integral a) => a -> PP.Doc
integralDoc = PP.integer <$> toInteger

pList :: PP.Pretty a => Text -> [a] -> PP.Doc
pList prefix as = PP.vsep $ indexedDoc <$> zipped
 where
  indexes = L.findIndices (const True) as
  zipped = L.zip indexes as
  prefixDoc = text prefix
  numberedDoc idx = prefixDoc <> text " #" <> integralDoc idx
  nestedDoc a = PP.nest 6 (PP.pretty a)
  indexedDoc (idx, a) = numberedDoc idx <> text ": " <> nestedDoc a

filterDevices :: ((OpenCLPlatform, OpenCLDevice) -> Bool) -> [OpenCLPlatform] -> [OpenCLDevice]
filterDevices f platforms = do
  platform <- platforms
  snd <$> L.filter f ((platform,) <$> platformDevices platform)

findDevice :: ((OpenCLPlatform, OpenCLDevice) -> Bool) -> [OpenCLPlatform] -> Maybe OpenCLDevice
findDevice f platforms = 
  case filterDevices f platforms of
    [] -> Nothing
    a:_ -> Just a

createOpenCLContext :: [OpenCLDevice] -> IO CLContext
createOpenCLContext devices = clCreateContext (CL_CONTEXT_PLATFORM . devicePlatformId <$> devices) (deviceId <$> devices) putStrLn

createOpenCLProgram :: CLContext -> Text -> IO CLProgram
createOpenCLProgram ctx txt = clCreateProgramWithSource ctx $ T.unpack txt

joinArgs :: [Text] -> String
joinArgs = T.unpack . T.unwords

buildOpenCLProgram :: CLProgram -> [OpenCLDevice] -> [Text] -> IO CLProgram
buildOpenCLProgram prog devices args = do
  res <- try (clBuildProgram prog (deviceId <$> devices) (joinArgs args))
  case res of
    Left (err :: CLError) -> do
        -- TODO: logger
        errs <- traverse (clGetProgramBuildLog prog) (deviceId <$> devices)
        traverse_ putStrLn errs
        throw err
    Right _ -> pure ()
  pure prog

createOpenCLKernel :: CLProgram -> Text -> IO CLKernel
createOpenCLKernel prog name = clCreateKernel prog $ T.unpack name

createOpenCLWorkQueue :: CLContext -> [CLCommandQueueProperty] -> OpenCLDevice -> IO OpenCLWorkQueue
createOpenCLWorkQueue ctx props dev = OpenCLWorkQueue dev <$> clCreateCommandQueue ctx (deviceId dev) props

prepareOpenCLWork :: Text -> [OpenCLDevice] -> [Text] -> Text -> IO OpenCLWork
prepareOpenCLWork source devs args kernelName = do
  context <- createOpenCLContext devs
  program <- createOpenCLProgram context source
  builtProgram <- buildOpenCLProgram program devs args
  kernel <- createOpenCLKernel builtProgram kernelName
  queues <- traverse (createOpenCLWorkQueue context []) devs
  let inBuf s p = clCreateBuffer context [CL_MEM_READ_WRITE, CL_MEM_COPY_HOST_PTR] (s, p)
  outBuf  <- clCreateBuffer context [CL_MEM_WRITE_ONLY] (8 :: Int, nullPtr)
  pure $ OpenCLWork context queues source builtProgram kernel inBuf outBuf

run :: GPUEnv -> TargetBytes -> HeaderBytes -> OpenCLWork -> IO Word64 -> IO MiningResult
run cfg (TargetBytes target) (HeaderBytes header) work genNonce = do
  startTime <- getCurrentTime
  offsetP <- calloc :: IO (Ptr CSize)
  resP <- calloc :: IO (Ptr Word64)
  let bufBytes = BS.unpack $ BS.append (pad 288 header) target
  bufArr <- newArray bufBytes
  !inBuf <- workPrepareInputBuf work 320 (castPtr bufArr)
  (T2 end steps) <- doIt offsetP resP inBuf work (1::Int)
  endTime <- getCurrentTime
  let numNonces = globalSize cfg * 64 * steps
  let !result = MiningResult
        (BSL.toStrict $ BSB.toLazyByteString $ BSB.word64LE end)
        (fromIntegral numNonces)
        (fromIntegral numNonces `div` max 1 (round (endTime `diffUTCTime` startTime)))
        mempty
  traverse_ clReleaseCommandQueue (workQueue <$> workQueues work)
  _ <- clReleaseContext (workContext work)
  free offsetP
  free resP
  pure result
 where
  doIt offsetP resP inBuf w@(OpenCLWork _ [queue] _ _ kernel _ resultBuf) !n = do
    nonce <- genNonce
    clSetKernelArgSto kernel 0 inBuf
    clSetKernelArgSto kernel 1 nonce
    e1 <- clEnqueueWriteBuffer (workQueue queue) resultBuf True (0::Int) 8 (castPtr resP) []
    e2 <- clEnqueueNDRangeKernel (workQueue queue) kernel [globalSize cfg] [localSize cfg] [e1]
    e3 <- clEnqueueReadBuffer (workQueue queue) resultBuf True (0::Int) 8 (castPtr resP) [e2]
    _ <- clWaitForEvents [e3]
    -- required to avoid leaking everything else
    traverse_ clReleaseEvent [e1,e2,e3]
    !res <- peek resP
    if res == 0 then 
      doIt offsetP resP inBuf w (n+1)
    else 
      return $ T2 res n
  doIt _ _ _ _ _ = error "using multiple devices at once is unsupported"

  pad :: Int -> ByteString -> ByteString
  pad desiredLength bs = 
    let missingLen = desiredLength - BS.length bs
     in BS.append bs (BS.replicate missingLen 0)

openCLMiner :: GPUEnv -> [OpenCLWork] -> TargetBytes -> HeaderBytes -> IO MiningResult
openCLMiner cfg works t h = do
  runningDevs <- traverse (\device -> async $ run cfg t h device randomIO) works
  results <- waitAny runningDevs
  pure $ snd results
