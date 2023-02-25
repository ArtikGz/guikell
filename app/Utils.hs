module Utils where

reverse' :: (a, b) -> (b, a)
reverse' (x, y) = (y, x)

tuple2 :: [a] -> (a, a)
tuple2 [a, b] = (a, b)

mapT :: (a -> b) -> (a, a) -> (b, b)
mapT p (t1, t2) = (p t1, p t2)