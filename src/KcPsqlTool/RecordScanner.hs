{-# LANGUAGE
    OverloadedStrings
  , NumericUnderscores
  , ScopedTypeVariables
  , DeriveGeneric
  , RecordWildCards
  #-}
module KcPsqlTool.RecordScanner where

import Control.DeepSeq
import Control.Exception.Safe
import Data.Aeson
import Data.Int
import Data.String
import Data.Time.Clock.System
import Filesystem.Path.CurrentOS
import GHC.Generics
import GHC.Stack (emptyCallStack)
import PostgreSQL.Binary.Data
import Prelude hiding (FilePath)
import System.IO hiding (FilePath)
import Turtle.Pattern
import Turtle.Prelude hiding (stderr)
import Turtle.Shell

import qualified Codec.Compression.GZip as GZ
import qualified Control.Foldl as Foldl
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Vector as Vec
import qualified Prelude (FilePath)

toText' :: FilePath -> T.Text
toText' = either id id . toText

getBattleRecordIds :: Prelude.FilePath -> IO [(Int64, FilePath)]
getBattleRecordIds fpRaw =
  reduce Foldl.list $ do
    fp <- ls (fromString fpRaw)
    let fName = toText' $ filename fp
    [battleId] <- pure $ match (decimal <* ".json.gz") fName
    pure (battleId, fp)

epochMillisecondsToUTCTime :: Int64 -> UTCTime
epochMillisecondsToUTCTime ms = systemToUTCTime st
  where
    (seconds, mills) = ms `quotRem` 1000
    st = MkSystemTime seconds (fromIntegral mills * 1_000_000)

data BattleRecord
  = BattleRecord
  { brId :: Int64
  , brVersion :: T.Text
  , brType :: T.Text
  , brMap :: Vec.Vector Int16
  , brDesc :: Maybe T.Text
  , brTime :: UTCTime
  , brFleet :: Value
  , brPacket :: Vec.Vector Value
  , brExtra :: Maybe Object -- a JSON object that holds other extra fields.
  } deriving (Generic, Show)

instance NFData BattleRecord

instance FromJSON BattleRecord where
  parseJSON = withObject "BattleRecord" $ \obj -> do
    (t :: Int64) <- obj .: "time"
    let extra = foldr HM.delete obj ["version", "type", "map", "desc", "fleet", "packet"]
    BattleRecord
      <$> pure t
      <*> obj .: "version"
      <*> obj .: "type"
      <*> obj .: "map"
      <*> obj .: "desc"
      <*> pure (epochMillisecondsToUTCTime t)
      <*> obj .: "fleet"
      <*> obj .: "packet"
      <*> pure (if HM.null extra then Nothing else Just extra)

instance ToJSON BattleRecord where
  toJSON BattleRecord {..} =
      maybe
        base
        -- explicit fields takes priority over extra key-value pairs.
        (Object . HM.union obj)
        brExtra
    where
      base@(Object obj) = object
        [ "version" .= brVersion
        , "type" .= brType
        , "map" .= brMap
        , "desc" .= brDesc
        , "time" .= brTime -- TODO: we need epoch milliseconds here.
        , "fleet" .= brFleet
        , "packet" .= brPacket
        ]

loadAndDecompress' :: Prelude.FilePath -> IO BS.ByteString
loadAndDecompress' fp = do
  h <- openFile fp ReadMode
  raw <- BSL.hGetContents h
  -- data for a record is relatively tiny, I'm fine with decompressing it all in-memory.
  let x = BSL.toStrict $ GZ.decompress raw
  x `seq` hClose h
  pure x

loadAndDecompress :: Prelude.FilePath -> IO (Either SomeException BS.ByteString)
loadAndDecompress fp =
  catchAny (Right <$> loadAndDecompress' fp) (pure . Left)

loadBattleRecord :: FilePath -> IO (Either SomeException BattleRecord)
loadBattleRecord fp = do
  mRaw <- loadAndDecompress (encodeString fp)
  case mRaw of
    Left e -> pure (Left e)
    Right raw ->
      catchAny
        (case eitherDecode' (BSL.fromStrict raw) of
            Left e -> pure (Left (toException $ StringException e emptyCallStack))
            Right r -> pure (Right r))
        (pure . Left)
