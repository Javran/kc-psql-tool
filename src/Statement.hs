{-# LANGUAGE
    OverloadedStrings
  , TemplateHaskell
  , QuasiQuotes
  #-}
module Statement where

import Data.Int
import Data.Profunctor
import Hasql.Statement
import Hasql.TH
import PostgreSQL.Binary.Data

import qualified Data.Vector as Vec
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders

import RecordScanner

{-
  TODO: Hasql.TH.
 -}

{-
  id: int8, same as time
  version: text
  type: text
  map: array of int2
  desciption: nullable text -- note: desc is a keyword.
  time: timestamptz
  fleet: jsonb
  packet: array of jsonb
 -}

createTable :: Statement () ()
createTable =
    Statement sql Encoders.noParams Decoders.noResult False
  where
    sql =
      "CREATE TABLE IF NOT EXISTS poi_battle_records (\
      \  id int8 PRIMARY KEY NOT NULL,\
      \  version text NOT NULL,\
      \  type text NOT NULL,\
      \  map int2 ARRAY NOT NULL,\
      \  description text,\
      \  time timestamptz NOT NULL,\
      \  fleet jsonb NOT NULL,\
      \  packet jsonb ARRAY NOT NULL\
      \)"

queryMissingRecords :: Statement (Vector Int64) (Vector Int64)
queryMissingRecords =
  [vectorStatement|
    SELECT tmp.id :: int8 FROM poi_battle_records AS rs
      RIGHT JOIN (
        SELECT * FROM UNNEST($1 :: int8[]) AS id
      ) AS tmp
      ON rs.id = tmp.id
      WHERE rs.id IS NULL
      |]

insertBattleRecord :: Statement BattleRecord ()
insertBattleRecord = lmap brToRows
  [resultlessStatement|
    INSERT INTO poi_battle_records
      (id, version, type, map, description, time, fleet, packet)
      VALUES
        ( $1 :: int8
        , $2 :: text
        , $3 :: text
        , $4 :: int2[]
        , $5 :: text?
        , $6 :: timestamptz
        , $7 :: jsonb
        , $8 :: jsonb[]
        ) ON CONFLICT DO NOTHING
        |]
  where
    brToRows br =
      ( brId br
      , brVersion br
      , brType br
      , Vec.fromList $ brMap br
      , brDesc br
      , brTime br
      , brFleet br
      , Vec.fromList $ brPacket br
      )
