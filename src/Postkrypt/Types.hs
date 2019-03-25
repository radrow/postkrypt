-- AUTHOR: Radosław Rowicki 386088
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE GADTs           #-}

module Postkrypt.Types where

import           Data.Bifunctor
import           Control.Monad.Except
import           Control.Monad.State
import           Mon


type R = Rational
type R2 = (R, R)


newtype Point = Point
  { pointCoords :: R2
  } deriving (Eq, Show, Ord)
point :: R2 -> Point
point = Point


newtype Vec = Vec
  { vecDims :: R2
  } deriving (Eq, Show, Ord)
vec :: R2 -> Vec
vec = Vec

instance Mon Vec where
  (><) = (<>)
  m1 = mempty

instance Semigroup Vec where
  (Vec (x, y)) <> (Vec b) = Vec $ bimap (+ x) (+ y) b

instance Monoid Vec where
  mempty = Vec (0, 0)
  mappend = (<>)


type Path = [Point]


newtype Picture = Picture
  { pathes :: [Path]
  } deriving (Eq, Show, Ord)

instance Mon Picture where
  (><) = (<>)
  m1 = mempty

instance Semigroup Picture where
  Picture s1 <> Picture s2 = Picture $ s1 <> s2

instance Monoid Picture where
  mempty = Picture mempty
  mappend = (<>)


type IntLine = ((Int, Int), (Int, Int))
type IntRendering = [IntLine]


data TransformUnit = Translate Vec | Rotate R
  deriving (Show, Eq)
newtype Transform = Transform [TransformUnit]
  deriving (Show, Eq)

instance Mon Transform where
  (><) = (<>)
  m1 = mempty

instance Semigroup Transform where
  Transform t1 <> Transform t2 = Transform $ simplify $ t1 <> t2 where
    simplify [] = []
    simplify [x] = [x]
    simplify (Translate a1 : Translate a2 : rest) = Translate (a1 <> a2) : simplify rest
    simplify (Rotate a1 : Rotate a2 : rest) = Rotate (a1 + a2) : simplify rest
    simplify (a : rest) = a : simplify rest
instance Monoid Transform where
  mappend = (<>)
  mempty = Transform mempty


type Output = String
type Stack = [R]
data ParsingState = ParsingState
  { pos              :: Maybe Point
  , stack            :: Stack
  , picture          :: Picture
  , currentTransform :: Transform
  }


type ErrorWrap = ExceptT String
type Parser = ErrorWrap (State ParsingState)
