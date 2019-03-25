-- AUTHOR: Radosław Rowicki 386088
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeApplications #-}

module Postkrypt.Picture where

import           Data.Bifunctor
import           Data.Fixed

import           Postkrypt.Types


line :: (R, R) -> (R, R) -> Picture
line = (Picture .) . flip flip ([]) . ((:) .) . (. (return . Point)) . (:) . Point


rectangle :: R -> R -> Picture
rectangle a b = Picture [fmap Point [(0, 0), (0, b), (a, b), (b, 0), (0, 0)]]


(&) :: Picture -> Picture -> Picture
(&) = (<>)


simplifyPath :: Path -> Path
simplifyPath = reverse . go [] where
  go acc [] = acc
  go acc [a] = a : acc
  go acc (a:b:rest) =
    if a == b
    then go acc (a : rest)
    else go (a : acc) (b : rest)


render :: Picture -> IntRendering
render = renderScaled 1


renderScaled :: Int -> Picture -> IntRendering
renderScaled scale pic =
  let rScale = fromIntegral scale
      toLines :: [a] -> [(a, a)]
      toLines l =
        if length l < 2
          then []
          else zip <$> init <*> tail $ l
      process = (fromInteger . round . (rScale *))
   in pathes pic >>=
      toLines .
      fmap ((bimap process process) . pointCoords) . reverse . simplifyPath


trpoint :: Transform -> Point -> Point
trpoint (Transform t) pnt = foldr apply pnt t where
  apply f (Point (a, b)) = Point $ case f of
    Translate (Vec (x, y)) -> (a + x, b + y)
    Rotate r -> let s = mysin r; c = mycos r
                in (c * a + (-s) * b, s * a + c * b)


trvec :: Transform -> Vec -> Vec
trvec (Transform t) vc = foldr apply vc t where
  apply f (Vec (a, b)) = Vec $ case f of
    Translate _ -> (a, b)
    Rotate r -> let s = mysin r; c = mycos r
                in (c * a + (-s) * b, s * a + c * b)


trpath :: Transform -> Path -> Path
trpath t points = fmap (trpoint t) points


transform :: Transform -> Picture -> Picture
transform t (Picture shapes) = Picture $ fmap (trpath t) shapes


translate :: Vec -> Transform
translate = Transform . pure . Translate

rotate :: R -> Transform
rotate = Transform . pure . Rotate

mysin :: (Real a, Fractional a) => a -> a
mysin angle = let forsin = angle `mod'` fullCircle
                  formula r = (4 * r * (180 - r)) / (40500 - r * (180 - r))
              in formula (forsin `mod'` 180) * if forsin > 180 then -1 else 1


mycos :: (Real a, Fractional a) => a -> a
mycos = mysin . (+90)


fullCircle :: (Real f, Fractional f) => f
fullCircle = 360
