{-# LANGUAGE DeriveGeneric, LambdaCase #-}
module KcPsqlTool.Config where

import Data.Text.Encoding (encodeUtf8)
import Hasql.Connection
import Control.Exception.Safe

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

{-
  TODO: battle data path should be an argument to the command line rather than being here.

  so we want to end up having:

  - a config that's shared among all tools, preferably through an environment variable as config file path.
  - for poi2psql, a list of importing sources, each one could be of a different format.
  - for psql-get, a list of ids, and probably an output directory.

 -}
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

withPsqlConnection :: PsqlConfig -> (Connection -> IO a) -> IO a
withPsqlConnection pConf =
  bracket
    (acquireFromConfig pConf <* putStrLn "connection acquired successfully.")
    (\conn -> putStrLn "releasing connection ..." >> release conn)
