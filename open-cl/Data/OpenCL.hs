{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Data.OpenCL(
  OpenCLDevice(..), 
  OpenCLPlatform(..), 
  queryAllOpenCLDevices,
  filterDevices,
  findDevice,
  fetchDeviceByIndex,
  createOpenCLContext,
  createOpenCLProgram,
  createOpenCLKernel,
  createOpenCLWorkQueue,
  buildOpenCLProgram,
  prepareOpenCLWork
) where 


import           Control.Parallel.OpenCL
import           Data.Text as T
import qualified Data.List as L
import           Foreign.C.Types
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import           Prelude as P

data OpenCLPlatform = OpenCLPlatform
  { platformId :: CLPlatformID
  , platformName :: Text
  , platformVersion :: Text
  , platformVendor :: Text
  , platformExtensions :: [Text]
  , platformDevices :: [OpenCLDevice]
  } deriving Show

data OpenCLDevice = OpenCLDevice
  { deviceId :: CLDeviceID
  , devicePlatformId :: CLPlatformID
  , deviceName :: Text
  , deviceVendor :: Text
  , deviceVersion :: Text
  , deviceTypes :: [CLDeviceType]
  , deviceAvailable :: Bool
  , deviceExtensions :: [Text]
  , deviceAddressBits :: CLuint
  , deviceGlobalMemSize :: CLulong
  , deviceGlobalCacheSize :: CLulong
  , deviceCacheLineSize :: CLuint
  , deviceLocalMemSize :: CLulong
  , deviceMaxClock :: CLuint
  , deviceComputeUnits :: CLuint
  , deviceMaxWorkGroupSize :: CSize
  , deviceMaxWorkItemSizes :: [CSize]
  } deriving Show

data OpenCLWork = OpenCLWork
  { workQueues :: [OpenCLWorkQueue]
  , workSource :: ProgramString
  , workProgram :: CLProgram
  , workKernel :: CLKernel
  } deriving Show

data OpenCLWorkQueue = OpenCLWorkQueue
  { workDevice :: OpenCLDevice
  , workContext :: CLContext
  , workqueue :: CLCommandQueue
  } deriving Show

type ProgramString = Text
type CompileArgs = [Text]
type KernelName = Text

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
buildOpenCLPlatform pId = 
 let platformInfo prop = pack <$> clGetPlatformInfo pId prop
  in do
    name <- platformInfo CL_PLATFORM_NAME
    version <- platformInfo CL_PLATFORM_VERSION
    vendor <- platformInfo CL_PLATFORM_VENDOR
    extensions <- splitOn " " <$> platformInfo CL_PLATFORM_EXTENSIONS
    deviceIds <- clGetDeviceIDs pId CL_DEVICE_TYPE_ALL
    devices <- traverse (buildDevice pId) deviceIds
    pure $ OpenCLPlatform pId name version vendor extensions devices

buildDevice :: CLPlatformID -> CLDeviceID -> IO OpenCLDevice
buildDevice pId dId = do 
  name <- pack <$> clGetDeviceName dId 
  version <- pack <$> clGetDeviceVersion dId
  vendor <- pack <$> clGetDeviceVendor dId
  tpe <- clGetDeviceType dId
  available <- clGetDeviceAvailable dId
  extensions <- splitOn " " . pack <$> clGetDeviceExtensions dId
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

filterDevices :: ((OpenCLPlatform, OpenCLDevice) -> Bool) -> [OpenCLPlatform] -> [OpenCLDevice]
filterDevices f platforms = do
  platform <- platforms
  snd <$> P.filter f ((platform,) <$> platformDevices platform)

findDevice :: ((OpenCLPlatform, OpenCLDevice) -> Bool) -> [OpenCLPlatform] -> Maybe OpenCLDevice
findDevice f platforms = 
  case filterDevices f platforms of
    [] -> Nothing
    a:_ -> Just a

fetchDeviceByIndex :: Int -> Int -> [OpenCLPlatform] -> Maybe OpenCLDevice
fetchDeviceByIndex platformIndex deviceIndex platforms = do
  platform <- elemAt platformIndex platforms
  elemAt deviceIndex (platformDevices platform)

elemAt :: Int -> [a] -> Maybe a
elemAt 0 (x:_) = Just x
elemAt n (_:t) = elemAt (n - 1) t
elemAt _ [] = Nothing

text :: Text -> PP.Doc
text = PP.text <$> unpack

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

createOpenCLContext :: [OpenCLDevice] -> IO CLContext
createOpenCLContext devices = clCreateContext (CL_CONTEXT_PLATFORM . devicePlatformId <$> devices) (deviceId <$> devices) putStrLn

createOpenCLProgram :: CLContext -> ProgramString -> IO CLProgram
createOpenCLProgram ctx txt = clCreateProgramWithSource ctx $ unpack txt

joinArgs :: [Text] -> String
joinArgs = unpack . T.unwords

buildOpenCLProgram :: CLProgram -> [OpenCLDevice] -> CompileArgs -> IO CLProgram
buildOpenCLProgram prog devices args = do
  clBuildProgram prog (deviceId <$> devices) (joinArgs args)
  pure prog

createOpenCLKernel :: CLProgram -> KernelName -> IO CLKernel
createOpenCLKernel prog name = clCreateKernel prog $ unpack name

createOpenCLWorkQueue :: CLContext -> [CLCommandQueueProperty] -> OpenCLDevice -> IO OpenCLWorkQueue
createOpenCLWorkQueue ctx props dev = OpenCLWorkQueue dev ctx <$> clCreateCommandQueue ctx (deviceId dev) props

prepareOpenCLWork :: ProgramString -> [OpenCLDevice] -> CompileArgs -> KernelName -> IO OpenCLWork
prepareOpenCLWork source devs args kernelName = do
  context <- createOpenCLContext devs
  program <- createOpenCLProgram context source
  builtProgram <- buildOpenCLProgram program devs args
  kernel <- createOpenCLKernel builtProgram kernelName
  queues <- traverse (createOpenCLWorkQueue context []) devs
  pure $ OpenCLWork queues source builtProgram kernel

