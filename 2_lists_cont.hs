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

--Problem 17
split :: [a] -> Int -> ([a], [a])
split x n = (firstPart x n, lastPart x n)
    where
    firstPart :: [a] -> Int -> [a]
    firstPart _ 0 = []
    firstPart [] n = error "Index out of range"
    firstPart (x:xs) n = x:(firstPart xs (n-1))
    
    lastPart :: [a] -> Int -> [a]
    lastPart x 0 = x
    lastPart [] n = error "Index out of range"
    lastPart (_:xs) n = lastPart xs (n-1)

--Problem 18
slice :: [a] -> Int -> Int -> [a]
slice [] _ _ = []
slice (x:xs) i j
    | i <= 1 && j >= 1 = x:(slice xs (i-1) (j-1))
    | otherwise        = slice xs (i-1) (j-1)

--Problem 19
rotate :: [a] -> Int -> [a]
rotate xs 0 = xs
rotate lst@(x:xs) n = rotate (xs ++ [x]) (n'-1)
    where n' = mod n (length lst)

--Problem 20
removeAt' :: Int -> [a] -> [a]
removeAt' _ [] = []
removeAt' 1 (x:xs) = xs
removeAt' n (x:xs) = x:(removeAt' (n-1) xs)

removeAt :: Int -> [a] -> (a, [a])
removeAt n lst = (lst !! (n-1), removeAt' n lst)
