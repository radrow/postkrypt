-- AUTHOR: Radosław Rowicki 386088
module Postkrypt.Print where

import           Postkrypt.Types


prolog, epilog, errMsg :: String
prolog = "300 400 translate\n"
epilog = "stroke showpage\n"
errMsg = "/Courier findfont 24 scalefont setfont 0 0 moveto (Error) show\n"


printFull :: IntRendering -> String
printFull i = fmap (\s -> prolog <> s <> epilog) printRendering i


printRendering :: IntRendering -> String
printRendering = (>>= printLine)


printLine :: IntLine -> String
printLine ((x1, y1), (x2, y2)) =
  show x1 <> " " <> show y1 <> " moveto " <> show x2 <> " " <> show y2 <>
  " lineto\n"
