{-# LANGUAGE
    LambdaCase
  #-}
module KcPsqlTool.PsqlGetMain where

import Control.Monad
import Data.Aeson
import Data.Char
import Data.Text.Encoding (decodeUtf8)
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
  {-
    Admittedly this is not the most intuitive argument syntax in the world.
    In future this will be turned into something like --ids='111,222,333', at which point doing
    things the current will make more sense.
   -}
  [rawIds] -> do
    ProgConfig
      { pcSqlConfig = sqlConfig
      } <- loadProgConfigFromEnv
    case readP_to_S (skipSpaces *> battleIdsP <* eof) rawIds of
      [(ids, "")] ->
        withPsqlConnection sqlConfig $ \conn -> do
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
      _ -> do
        putStrLn "Failed to parse ids."
        exitFailure
  _ -> do
    putStrLn "psql-get <ids>"
    putStrLn "  where <ids> are comma-separated list of ids."
    exitFailure
