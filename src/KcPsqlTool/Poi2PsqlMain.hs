{-# LANGUAGE LambdaCase #-}
module KcPsqlTool.Poi2PsqlMain
  ( main
  ) where

import Control.Exception.Safe (displayException)
import Control.Monad
import Data.Text.Encoding (encodeUtf8)
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

acquireFromConfig :: PsqlConfig -> IO Connection
acquireFromConfig (PsqlConfig hst pt u pw db) =
    acquire sqlSettings >>= \case
      Left e -> do
        putStrLn "error while connecting to database."
        print e
        exitFailure
      Right conn -> pure conn
  where
    sqlSettings =
      settings
        (encodeUtf8 hst)
        (fromIntegral pt)
        (encodeUtf8 u)
        (encodeUtf8 pw)
        (encodeUtf8 db)

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
    let records = M.fromList recordsPre
    conn <- acquireFromConfig sqlConfig
    putStrLn "connection acquired successfully."
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
    putStrLn "releasing connection ..."
    release conn
  _ -> do
    putStrLn "poi2psql <config.dhall>"
    exitFailure
