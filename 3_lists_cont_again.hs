import System.Random
import Data.List hiding (group)

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

rnd_select :: [a] -> Int -> IO [a] --A useful helper function
rnd_select xs n = do
    gen <- getStdGen
    return $ take n [xs !! x | x <- randomRs (0, (length xs) - 1) gen]

--Problem 24

removeArr ::  Int -> [a] -> [a]
removeArr 0 (x:xs) = xs
removeArr n (x:xs) = x:(removeArr (n-1) xs)

diff_select :: Int -> Int -> IO [Int]
diff_select n max = go n [1..max]
    where
    go :: Int -> [Int] -> IO [Int]
    go 0 _ = return []
    go n lst = do
        gen <- getStdGen
        let idx = fst $ randomR (0, (length lst) - 1) gen
        xs <- go (n-1) (removeArr idx lst)
        return $ (lst !! idx):xs

--Problem 25
rnd_permu :: [a] -> IO [a] --Very similar to the previous one
rnd_permu [] = return []
rnd_permu lst = do
    gen <- getStdGen
    let idx = fst $ randomR (0, (length lst) - 1) gen
    xs <- rnd_permu (removeArr idx lst)
    return $ (lst !! idx):xs

--Problem 26
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations n [] = []
combinations n (x:xs) = map (x:) (combinations (n-1) xs) ++ (combinations n xs)


--Problem 27
combinations' :: Int -> [a] -> [([a],[a])]
combinations' 0 xs = [([],xs)]
combinations' n [] = []
combinations' n (x:xs) = lc1 ++ lc2
    where
    lc1 = [((x:ys), zs) | (ys, zs) <- combinations' (n-1) xs]
    lc2 = [(ys, (x:zs)) | (ys, zs) <- combinations' n xs]

group :: [Int] -> [a] -> [[[a]]]
group (n:ns) [] = []
group [] xs = [[]]
group (n:ns) xs = [ (fst y):ys | y <- comb, ys <- (group ns (snd y))]
    where
    comb = (combinations' n xs)

--Problem 28

--Part (a)

lsort :: (Ord a) => [[a]] -> [[a]]
lsort = sortBy (\x y -> compare (length x) (length y))

lf :: [a] -> [[a]] -> Int
lf i [] = 0
lf i (x:xs)
    | (length i) == (length x) = 1 + (lf i xs)
    | otherwise                = (lf i xs)

--Part (b)
lfsort :: (Ord a) => [[a]] -> [[a]]
lfsort arr = sortBy (\x y -> compare (lf x arr) (lf y arr)) arr
