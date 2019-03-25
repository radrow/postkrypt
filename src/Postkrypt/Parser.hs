-- AUTHOR: Radosław Rowicki 386088
{-# LANGUAGE LambdaCase #-}

module Postkrypt.Parser where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Except
import           Text.Read

import           Postkrypt.Picture
import           Postkrypt.StateManager
import           Postkrypt.Types


readNumber :: String -> Parser R
readNumber =
  maybe (throwError "Number parse error") (pure . fromInteger) . readMaybe


parseNumber :: String -> Parser ()
parseNumber s = readNumber (dropWhile (=='+') s) >>= pushNumber


parseOp :: String -> Parser ()
parseOp = \case
  "add" -> uncurry (+) <$> popNumber2 >>= pushNumber
  "sub" -> uncurry (-) <$> popNumber2 >>= pushNumber
  "mul" -> uncurry (*) <$> popNumber2 >>= pushNumber
  "div" -> popNumber2 >>= \(a, b) ->
    when (b == 0) (throwError "Division by 0") >> pushNumber (a / b)
  "moveto" -> popNumber2 >>= moveTo . Point
  "lineto" -> popNumber2 >>= lineTo . Point
  "closepath" -> closePath
  "translate" -> popNumber2 >>= addTransform . translate . Vec
  "rotate" -> popNumber >>= addTransform . rotate
  s -> throwError $ "Invalid token: " <> s


parseToken :: String -> Parser ()
parseToken s = parseNumber s <|> parseOp s


parseFile :: String -> Parser ()
parseFile s = forM_ (words s) parseToken
