--Problem 11
data Cardinality a = Single a | Multiple Int a
    deriving (Show)

getCard :: a -> Int -> Cardinality a
getCard x 1 = Single x
getCard x n = Multiple n x

encodeModified :: Eq a => [a] -> [Cardinality a]
encodeModified (x:xs) = go x 1 xs
    where
    go :: Eq a => a -> Int -> [a] -> [Cardinality a]
    go y n [] = [getCard y n]
    go y n (z:zs)
        | y == z    = go y (n+1) zs
        | otherwise = (getCard y n):(go z 1 zs)

--Problem 12
decodeModified :: Eq a => [Cardinality a] -> [a]
decodeModified [] = []
decodeModified ((Single x):xs) = x:(decodeModified xs)
decodeModified ((Multiple n x):xs) = (take n (repeat x)) ++ decodeModified xs

--Problem 13: Implemented the same was as number 11

--Problem 14
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x:x:(dupli xs)

--Problem 15
repli :: [a] -> Int -> [a]
repli [] _ = []
repli (x:xs) n = take n (repeat x) ++ repli xs n

--Problem 16
dropEvery :: [a] -> Int -> [a]
dropEvery x n = go x n 1
    where
    go :: [a] -> Int -> Int -> [a]
    go [] _ _ = []
    go (y:ys) n m
        | n == m    = go ys n 1
        | otherwise = y:(go ys n (m+1))

