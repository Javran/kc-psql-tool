{-# LANGUAGE LambdaCase #-}
module KcPsqlTool.Poi2PsqlMain
  ( main
  ) where

import Control.Exception.Safe (displayException)
import Control.Monad
import Dhall hiding (record)
import Hasql.Connection
import Hasql.Session
import System.Environment
import System.Exit

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
