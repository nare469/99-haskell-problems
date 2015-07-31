--Problem 31
isPrime :: Int -> Bool
isPrime 1 = error "One"
isPrime n = chkPrime n 2
    where
    chkPrime :: Int -> Int -> Bool
    chkPrime n m
        | n == m       = True
        | mod n m == 0 = False
        | otherwise    = chkPrime n (m+1)

--Problem 32
myGCD :: Int -> Int -> Int
myGCD 0 y = y
myGCD x 0 = x
myGCD x y = myGCD (mod a b) b
    where
    a = abs (max x y)
    b = abs (min x y)

--Problem 33
coprime :: Int -> Int -> Bool
coprime x y = (myGCD x y) == 1

--Problem 34
totient :: Int -> Int
totient n = go n 1
    where
    go :: Int -> Int -> Int
    go n m
        | n == m        = 0
        | (coprime n m) = 1 + (go n (m+1))
        | otherwise     = go n (m+1)

--Problem 35
primeFactors :: Int -> [Int]
primeFactors n = go n 2
    where
    go :: Int -> Int -> [Int]
    go 1 _ = []
    go n m
        | (myGCD n m) == m = m:(go (n `div` m) 2)
        | otherwise        = go n (m+1)
        
