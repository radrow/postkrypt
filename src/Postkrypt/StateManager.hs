-- AUTHOR: Radosław Rowicki 386088
{-# LANGUAGE LambdaCase #-}

module Postkrypt.StateManager where

import           Control.Monad.Except
import           Control.Monad.State

import           Postkrypt.Picture
import           Postkrypt.Types


emptyState :: ParsingState
emptyState =
  ParsingState
    { pos = Nothing
    , stack = []
    , picture = Picture []
    , currentTransform = mempty
    }


runParser :: ParsingState -> Parser a -> Either String a
runParser s p = evalState (runExceptT p) s


pushNumber :: R -> Parser ()
pushNumber r = modify $ \s -> s{stack=r : stack s}


popNumber :: Parser R
popNumber = gets stack >>= \case
  [] -> throwError "Stack exhausted"
  (h:t) -> modify (\s -> s{stack = t}) >> pure h


popNumber2 :: Parser (R, R)
popNumber2 = flip (,) <$> popNumber <*> popNumber


moveTo :: Point -> Parser ()
moveTo p = do
  tr <- gets currentTransform
  modify $ \s -> s{ picture = Picture . ([trpoint tr p] :) . pathes $ picture s
                  , pos = Just $ trpoint tr p}


lineTo :: Point -> Parser ()
lineTo p = do
  tr <- gets currentTransform
  gets (pathes . picture) >>= \case
    [] -> throwError "No line beginning"
    (h:rest) -> modify $ \s -> s{ picture = Picture ((trpoint tr p : h) : rest)
                                , pos = Just $ trpoint tr p}


closePath :: Parser ()
closePath = do
  gets (pathes . picture) >>= \case
    [] -> pure ()
    ([]:_) -> throwError "No path to close"
    (h:rest) -> do
      modify $ \s -> s{pos = Just $ last h}
      modify $ \s -> s{picture = Picture $ (last h : h) : rest}


addTransform :: Transform -> Parser ()
addTransform t = modify $ \s -> s{currentTransform = t <> currentTransform s}
