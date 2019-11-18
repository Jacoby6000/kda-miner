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
  buildOpenCLProgram,
  createOpenCLKernel
) where 


import           Control.Parallel.OpenCL
import           Data.Text
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
  }

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
  }

instance PP.Pretty OpenCLPlatform where
  prettyList = pList "Platform"
  pretty (OpenCLPlatform _ _ version _ _ devices) =
    text version <> PP.hardline <> PP.prettyList devices <> PP.hardline


instance PP.Pretty OpenCLDevice where 
  prettyList = pList "Device"
  pretty (OpenCLDevice _ _ name _ _ tpe ava _ addressBits globalMemSize globalCacheSize _ localMemSize maxClock computeUnits workGroupSize itemSize) =
    text name <> PP.hardline <> 
      PP.vsep [
        text "Type:                " <> PP.text (show tpe),
        text "Bus Width:           " <> integralDoc addressBits, 
        text "Total Memory:        " <> integralDoc globalMemSize,
        text "Local Memory:        " <> integralDoc localMemSize,
        text "Total Cache:         " <> integralDoc globalCacheSize,
        text "Max Clock Speed:     " <> integralDoc maxClock <> text " MHz",
        text "Compute Units:       " <> integralDoc computeUnits,
        text "Max Work Group Size: " <> integralDoc workGroupSize,
        text "Item sizes:          " <> PP.encloseSep PP.empty PP.empty PP.comma (integralDoc <$> itemSize),
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

integralDoc :: (Integral a) => a -> PP.Doc 
integralDoc = PP.integer <$> toInteger
   
pList :: PP.Pretty a => Text -> [a] -> PP.Doc
pList prefix as = PP.vsep $ indexedDoc <$> zipped
 where 
  indexes = L.findIndices (const True) as
  zipped = L.zip indexes as
  prefixDoc = text prefix
  indexedDoc (idx, a) = prefixDoc <> text " #" <> integralDoc idx <> text " " <> PP.nest 6 (PP.pretty a)

createOpenCLContext :: [OpenCLDevice] -> IO CLContext
createOpenCLContext devices = clCreateContext (CL_CONTEXT_PLATFORM . devicePlatformId <$> devices) (deviceId <$> devices) putStrLn

createOpenCLProgram :: CLContext -> String -> IO CLProgram
createOpenCLProgram = clCreateProgramWithSource

buildOpenCLProgram :: CLProgram -> [OpenCLDevice] -> String -> IO CLProgram
buildOpenCLProgram prog devices args = do
  clBuildProgram prog (deviceId <$> devices) args 
  pure prog

createOpenCLKernel :: CLProgram -> String -> IO CLKernel
createOpenCLKernel = clCreateKernel

