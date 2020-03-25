{-# LANGUAGE
    LambdaCase
  #-}
module KcPsqlTool.PsqlGetMain where

import Control.Monad
import Data.Aeson
import Data.Char
import Data.Text.Encoding (decodeUtf8)
import Dhall
import Hasql.Connection
import Hasql.Session
import System.Environment
import System.Exit
import Text.ParserCombinators.ReadP

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Vector as Vec
import qualified Data.Text.IO as T

import KcPsqlTool.Config

import qualified KcPsqlTool.Statement as Statement

battleIdsP :: ReadP [Int]
battleIdsP =
  (read <$> (munch1 isDigit <* skipSpaces)) `sepBy1` (char ',' >> skipSpaces)

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
          Right rs ->
            forM_ rs $ \br -> do
              putStrLn "==== BEGIN"
              T.putStrLn . decodeUtf8 . BSL.toStrict . encode $ br
              putStrLn "==== END"
        putStrLn "releasing connection ..."
        release conn
      _ -> do
        putStrLn "Failed to parse ids."
        exitFailure
  _ -> do
    putStrLn "psql-get <config.dhall> <ids>"
    putStrLn "  where <ids> are comma-separated list of ids."
    exitFailure
