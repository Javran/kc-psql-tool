{-# LANGUAGE
    LambdaCase
  #-}
module KcPsqlTool.PsqlGetMain where

import Data.Char
import Dhall
import System.Environment
import System.Exit
import Text.ParserCombinators.ReadP
import Hasql.Connection
import Hasql.Session
import Data.Text.Encoding (encodeUtf8)

import qualified Data.Vector as Vec

import KcPsqlTool.Config
import KcPsqlTool.RecordScanner

import qualified KcPsqlTool.Statement as Statement

battleIdsP :: ReadP [Int]
battleIdsP =
  (read <$> (munch1 isDigit <* skipSpaces)) `sepBy1` (char ',' >> skipSpaces)

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

main :: IO ()
main = getArgs >>= \case
  [configPath, rawIds] ->
    case readP_to_S (skipSpaces *> battleIdsP <* eof) rawIds of
      [(ids, "")] -> do
        ProgConfig
          { pcSqlConfig = sqlConfig
          } <- inputFile auto configPath
        -- fetch battle records
        conn <- acquireFromConfig sqlConfig
        putStrLn "connection acquired successfully."
        let sess =
              statement
                (Vec.fromList $ fmap fromIntegral ids)
                Statement.selectRecordsById
        run sess conn >>= \case
          Left qe -> do
            putStrLn "query error"
            print qe
          Right rs -> do
            mapM_ print rs
        putStrLn "releasing connection ..."
        release conn
      _ -> do
        putStrLn "Failed to parse ids."
        exitFailure
  _ -> do
    putStrLn "psql-get <config.dhall> <ids>"
    putStrLn "  where <ids> are comma-separated list of ids."
    exitFailure
