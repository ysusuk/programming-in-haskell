module DS where

myLookup :: Eq a => a -> [(a, b)] -> Maybe b
myLookup _ [] = Nothing
myLookup key ((thiskey, thisvalue):rest) =
  if key == thiskey
    then Just thisvalue
    else lookup key rest
