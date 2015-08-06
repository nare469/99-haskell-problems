and' :: Bool -> Bool -> Bool
and' True True = True
and' _ _ = False

or' :: Bool -> Bool -> Bool
or' False False = False
or' _ _ = True

not' :: Bool -> Bool
not' True = False
not' False = True

--Important helper
(.*) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
(.*) = (.) . (.)

nand' :: Bool -> Bool -> Bool
nand' = not' .* and'

nor' :: Bool -> Bool -> Bool
nor' = not' .* or'

impl' :: Bool -> Bool -> Bool
impl' True False = False
impl' _ _ = True

xor' :: Bool -> Bool -> Bool
xor' = (/=)

equ' :: Bool -> Bool -> Bool
equ' = (==)

table :: (Bool -> Bool -> Bool) -> IO ()
table f = putStrLn $ unlines [(show x) ++ " " ++ (show y) ++ " " ++ (show (f x y)) | x <- [True, False], y <- [True, False]] 
