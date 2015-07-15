--Problem 1
myLast :: [a] -> a
myLast [] = error "Empty List"
myLast (x:[]) = x
myLast (_:xs) = myLast xs

--Problem 2
myButLast :: [a] -> a
myButLast [] = error "Empty List"
myButLast (x:[]) = error "Singleton List"
myButLast (x:_:[]) = x
myButLast (_:xs) = myButLast xs

--Problem 3
elementAt :: [a] -> Int -> a
elementAt [] _ = error "Index out of range"
elementAt (x:xs) 1 = x
elementAt (_:xs) n = elementAt xs (n-1)

--Problem 4
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

--Problem 5
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]

--Problem 6
isPalindrome :: Eq a => [a] -> Bool
isPalindrome x = (x == myReverse x)

--Problem 7
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List y) = (foldr (++) [] (map (flatten) y))

--Problem 8
compress :: Eq a => [a] -> [a]
compress [] = []
compress (x:xs) = (x:(go x xs))
    where
    go :: Eq a => a -> [a] -> [a]
    go _ [] = []
    go p (y:ys)
        | p == y    = go p ys
        | otherwise = (y:(go y ys))

--Problem 9
pack :: Eq a => [a] -> [[a]]
pack [] = [[]]
pack (x:xs) = go [[x]] xs x
    where
    go :: Eq a => [[a]] -> [a] -> a -> [[a]]
    go lst [] _ = lst
    go lst (y:ys) z
        | y == z    = go ((init lst) ++ (last lst ++ [y])) ys y
        | otherwise = go (lst ++ [[y]]) ys y

