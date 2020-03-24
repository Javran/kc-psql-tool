{-# LANGUAGE
    LambdaCase
  #-}
module KcPsqlTool.PsqlGetMain where

import Data.Char
import System.Environment
import System.Exit
import Text.ParserCombinators.ReadP

battleIdsP :: ReadP [Int]
battleIdsP =
  (read <$> (munch1 isDigit <* skipSpaces)) `sepBy1` (char ',' >> skipSpaces)

main :: IO ()
main = getArgs >>= \case
  [_configPath, rawIds] ->
    case readP_to_S (skipSpaces *> battleIdsP <* eof) rawIds of
      [(ids, "")] -> print ids
      _ -> do
        putStrLn "Failed to parse ids."
        exitFailure
  _ -> do
    putStrLn "psql-get <config.dhall> <ids>"
    putStrLn "  where <ids> are comma-separated list of ids."
    exitFailure
