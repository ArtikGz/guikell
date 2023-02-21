module Utils where

reverse' :: (a, b) -> (b, a)
reverse' (x, y) = (y, x)

tuple2 :: [a] -> (a, a)
tuple2 [a, b] = (a, b)