--Problem 21
insertAt :: a -> [a] -> Int -> [a]
insertAt y lst 1 = y:lst
insertAt y [] _ = error "Index out of bounds"
insertAt y (x:xs) n = x:(insertAt y xs (n-1))

--Problem 22
range :: Int -> Int -> [Int]
range x y
    | x > y     = error "Second number must be greater than the first"
    | x == y    = [x]
    | otherwise = x:(range (x+1) y)

--Problem 23

