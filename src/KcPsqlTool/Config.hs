{-# LANGUAGE DeriveGeneric, LambdaCase #-}
module KcPsqlTool.Config where

import Data.Text.Encoding (encodeUtf8)
import Hasql.Connection

import Dhall

data PsqlConfig
  = PsqlConfig
  { host :: Text
  , port :: Natural
  , user :: Text
  , password :: Text
  , database :: Text
  } deriving (Generic)

instance FromDhall PsqlConfig

-- TODO: we should probably verify that pcTableName is nothing fancy.
-- TODO: current nesting is not actually necessary.

data ProgConfig
  = ProgConfig
  { pcSqlConfig :: PsqlConfig
  , pcBattleDataPath :: FilePath
  } deriving (Generic)

instance FromDhall ProgConfig

acquireFromConfig :: PsqlConfig -> IO Connection
acquireFromConfig (PsqlConfig hst pt u pw db) =
    acquire sqlSettings >>= \case
      Left e ->
        error $ "error while connecting to database: " <> show e
      Right conn -> pure conn
  where
    sqlSettings =
      settings
        (encodeUtf8 hst)
        (fromIntegral pt)
        (encodeUtf8 u)
        (encodeUtf8 pw)
        (encodeUtf8 db)
