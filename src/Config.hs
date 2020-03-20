{-# LANGUAGE DeriveGeneric #-}
module Config where

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
