{-# LANGUAGE DeriveGeneric      #-}
module Miner.Chainweb 
  (
    HeaderBytes(..)
  , TargetBytes(..)
  , ChainBytes(..)
  , WorkBytes(..)
  , BlockHeight(..)
  , NodeInfo(..)
  , ChainId
  , encodeChainId
  , decodeChainId 
  , chainIdInt
  , runGet
  , decodeBlockHeight
  , unWorkBytes
) where

import           Control.Exception.Safe
import           Data.Aeson
import           Data.Bifunctor 
import           Data.Bytes.Put
import           Data.Bytes.Get
import           Data.Bytes.Signed (unsigned)
import qualified Data.ByteString.Char8 as B

import           Data.Serialize.Get (Get)
import           Data.Text
import           Data.Tuple.Strict (T3(..))
import           Data.Word
import           GHC.Generics

newtype HeaderBytes = HeaderBytes { _headerBytes :: B.ByteString }
newtype TargetBytes = TargetBytes B.ByteString
newtype ChainBytes = ChainBytes B.ByteString
newtype WorkBytes = WorkBytes B.ByteString
newtype BlockHeight = BlockHeight { height :: Word64 }


type ChainId = Word32

newtype DecodeException = DecodeException Text deriving (Show, Eq, Ord, Generic)
instance Exception DecodeException

data NodeInfo = NodeInfo
  { nodeVersion :: Text
  , nodeApiVersion :: Text
  , nodeChains :: [Text]
  , nodeNumberOfChains :: !Int
  } deriving (Generic)
instance FromJSON NodeInfo

chainIdInt :: Integral i => ChainId -> i
chainIdInt = fromIntegral
{-# INLINE chainIdInt #-}

 
encodeChainId :: MonadPut m => ChainId -> m ()
encodeChainId i32 = putWord32le $ unsigned i32
{-# INLINE encodeChainId #-}

decodeChainId :: MonadGet m => m ChainId
decodeChainId = getWord32le
{-# INLINE decodeChainId #-}

runGet :: MonadThrow m => Get a -> B.ByteString -> m a
runGet g = fromEitherM . runGetEither g
{-# INLINE runGet #-}

-- | Decode a value from a 'B.ByteString' and return either the result or a
-- 'DecodeException'.
--
runGetEither :: Get a -> B.ByteString -> Either DecodeException a
runGetEither g = first (DecodeException . pack) . runGetS g
{-# INLINE runGetEither #-}

fromEitherM :: MonadThrow m => Exception e => Either e a -> m a
fromEitherM = either throwM return
{-# INLINE fromEitherM #-}

decodeBlockHeight :: MonadGet m => m BlockHeight
decodeBlockHeight = BlockHeight <$> getWord64le

unWorkBytes :: WorkBytes -> T3 ChainBytes TargetBytes HeaderBytes
unWorkBytes (WorkBytes w) = T3 (ChainBytes c) (TargetBytes t) (HeaderBytes h)
  where
    (c, (t, h)) = second (B.splitAt 32) $ B.splitAt 4 w

