module Mon where

infixl 5 ><

class Mon m where
  (><) :: m -> m -> m
  m1 :: m
