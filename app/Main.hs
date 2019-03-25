-- AUTHOR: Radosław Rowicki 386088
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Monad.State
import           System.Environment
import           System.IO
import           Text.Read

import           Lib


helpMsg :: String
helpMsg = "USAGE: ls [SCALE]"


runWithScale :: Int -> IO ()
runWithScale s = do
  input <- getContents
  case runParser emptyState $
       fmap (printFull . renderScaled s) $ parseFile input >> gets picture of
    Left err -> hPutStrLn stderr err >> putStrLn errMsg
    Right out  -> putStrLn out


main :: IO ()
main = getArgs >>= \case
  [] -> runWithScale 1
  [nstr] -> case readMaybe nstr of
    Nothing         -> hPutStrLn stderr helpMsg
    Just (n :: Int) -> runWithScale n
  _ -> hPutStrLn stderr helpMsg
