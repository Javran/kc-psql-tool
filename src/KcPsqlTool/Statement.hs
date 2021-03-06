{-# LANGUAGE
    OverloadedStrings
  , TemplateHaskell
  , QuasiQuotes
  , RecordWildCards
  #-}
module KcPsqlTool.Statement where

import Data.Int
import Data.Profunctor
import Hasql.Statement
import Hasql.TH
import PostgreSQL.Binary.Data

import qualified Data.Aeson as Aeson
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders

import KcPsqlTool.RecordScanner

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
      \  packet jsonb ARRAY NOT NULL,\
      \  extra jsonb\
      \)"

{-
  In case that the table is created with "extra" field:

  ALTER TABLE poi_battle_records ADD COLUMN extra jsonb;

 -}

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
insertBattleRecord = lmap brToRow
    [resultlessStatement|
      INSERT INTO poi_battle_records
        (id, version, type, map, description, time, fleet, packet, extra)
        VALUES
          ( $1 :: int8
          , $2 :: text
          , $3 :: text
          , $4 :: int2[]
          , $5 :: text?
          , $6 :: timestamptz
          , $7 :: jsonb
          , $8 :: jsonb[]
          , $9 :: jsonb?
          ) ON CONFLICT DO NOTHING
          |]
  where
    brToRow =
      (,,,,,,,,)
        <$> brId
        <*> brVersion
        <*> brType
        <*> brMap
        <*> brDesc
        <*> brTime
        <*> brFleet
        <*> brPacket
        <*> (fmap Aeson.Object . brExtra)

selectRecordsById :: Statement (Vector Int64) (Vector BattleRecord)
selectRecordsById = rmap (fmap rowToBr)
    [vectorStatement|
      SELECT
        rs.id :: int8,
        version :: text,
        type :: text,
        map :: int2[],
        description :: text?,
        time :: timestamptz,
        fleet :: jsonb,
        packet :: jsonb[],
        extra :: jsonb?
        FROM poi_battle_records AS rs
          INNER JOIN (SELECT * FROM UNNEST($1 :: int8[]) AS id) AS tmp
          ON rs.id = tmp.id
          |]
  where
    {-
      Note that we skipped two validations here and just assume they are true:
      - extra column, if not null, should be an object rather than a flat value or array.
      - id and time is basically the same data represented differently.
     -}
    rowToBr
        ( brId, brVersion, brType, brMap
        , brDesc, brTime, brFleet, brPacket, extra')
        = BattleRecord {..}
      where
        brExtra = fmap (\(Aeson.Object obj) -> obj) extra'
