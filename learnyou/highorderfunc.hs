multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

-- let multTwoWithNine = multThree 9
-- multTwoWithNine 2 3 = 54

compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred x = compare 100 x

compareWithHundred' :: (Num a, Ord a) => a -> Ordering
compareWithHundred' = compare 100

divideByTen :: (Floating a) => a -> a
divideByTen = (/10) -- sectioned infix function by surrounding with parens and supply arg on one side
-- call with divideByTen 200
-- (/10) 200 is the same thing

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])

subtractFour :: Integer -> Integer
subtractFour = (subtract 4)
-- -4 means minus four for convenience


applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- applyTwice (multThree 2 2) 9
-- applyTwice (3:) [1]

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys
-- zipWith' max [6,3,2,1] [7,5,3,2]


flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
    where g x y = f y x
-- or
-- flip' f y x = f x y

-- zipWith (flip' div) [2,2..] [10,8,6,4,2]

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
    | p x = x : filter p xs
    | otherwise = filter p xs

-- filter (`elem` ['a'..'z']) "hI TheRe"

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort (filter (<=x) xs)
        biggerSorted = quicksort (filter (>x) xs)
    in smallerSorted ++ [x] ++ biggerSorted
