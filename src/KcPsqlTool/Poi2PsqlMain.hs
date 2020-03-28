{-# LANGUAGE LambdaCase #-}
module KcPsqlTool.Poi2PsqlMain
  ( main
  ) where

import Control.Exception.Safe (displayException)
import Control.Monad
import Dhall hiding (record)
import Hasql.Session
import System.Environment
import System.Exit
import Text.ParserCombinators.ReadP as ReadP

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Vector as Vec

import KcPsqlTool.Config
import KcPsqlTool.RecordScanner

import qualified KcPsqlTool.Statement as Statement

{-
  workflow:

  - scan and collect filenames from poi battle directory.
  - query database to see which of them are new records.
  - insert records into the database.
 -}

{-
  TODO: accept multiple sources:

  - new args:

    poipsql [source0] [source1] ...

    where a source is:

    - "BattleRecordPath:<path to battle record dir>"
    - "JsonLinesXz:<path to a jsonline.xz file>"

    Note: JsonLines is a custom format describing multiple json
    objects at once, in which every line (break by newline)
    is a json object. and suffix "Xz" stands for xz format
    (namely the file is expected to be *.jsonlines.xz)

   TODO: support for unpacking jsonline.xz is not yet implemented.
 -}

data RecordSource
  = BattlePath FilePath
  | JsonLinesXz FilePath

_parseRecordSource :: String -> Maybe RecordSource
_parseRecordSource raw = do
    [(v, "")] <- pure $ readP_to_S (recordSource <* eof) raw
    pure v
  where
    recordSource :: ReadP RecordSource
    recordSource =
      (BattlePath <$> (ReadP.string "BattleRecordPath:" *> munch1 (const True)))
      <++ (JsonLinesXz <$> (ReadP.string "JsonLineXz:" *> munch1 (const True)))

main :: IO ()
main = getArgs >>= \case
  [configPath] -> do
    ProgConfig
      { pcSqlConfig = sqlConfig
      , pcBattleDataPath = fp
      } <- inputFile auto configPath
    -- fetch battle records
    recordsPre <- getBattleRecordIds fp
    withPsqlConnection sqlConfig $ \conn -> do
      let records = M.fromList recordsPre
      -- create the table
      do
        let sess = statement () Statement.createTable
        run sess conn >>= \case
          Left qe -> do
            putStrLn "query error"
            print qe
          Right _ -> pure ()
      do
        putStrLn $ "record count: " <> show (length recordsPre)
        let sess =
              statement
                (Vec.fromList $ fst <$> recordsPre)
                Statement.queryMissingRecords
        run sess conn >>= \case
          Left qe -> do
            putStrLn "query error"
            print qe
          Right rIdsPre -> do
            putStrLn $ "missing records count: " <> show (length rIdsPre)
            -- importing all at once sounds like a terrible idea for testing,
            -- so instead let's just import a small bit and ramp it up if all goes well.
            let (rIds, dropped) = splitAt 20000 (Vec.toList rIdsPre)
            unless (null dropped) $
              putStrLn $ "inserting only first " <> show (length rIds) <> " records."
            let missingRecords = M.restrictKeys records (S.fromList rIds)
            forM_ (M.toList missingRecords) $ \(_rId, rPath) ->
              loadBattleRecord rPath >>= \case
                Left e -> do
                  putStrLn $ "Failed to load " <> show rPath
                  putStrLn $ "Exception: " <> displayException e
                Right record -> do
                  let insertSess = statement record Statement.insertBattleRecord
                  run insertSess conn >>= \case
                    Left se -> do
                      putStrLn "insertion error"
                      print se
                    Right () -> pure ()
  _ -> do
    putStrLn "poi2psql <config.dhall>"
    exitFailure
