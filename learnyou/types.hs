lucky :: (Integral a) => a -> String
lucky 7 = "Yay go you."
lucky x = "Not so good."

sayMe :: (Integral a) => a -> String
sayMe 1 = "One"
sayMe 2 = "Two"
sayMe 3 = "Three"
sayMe 4 = "Four"
sayMe 5 = "Five"
sayMe x = "Not between one and five"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

fstTriplet :: (a, b, c) -> a
fstTriplet (x, _, _) = x
sndTriplet :: (a, b, c) -> b
sndTriplet (_, y, _) = y
lstTriplet :: (a, b, c) -> c
lstTriplet (_, _, z) = z

--pattern match in list comprehension
--let xs = [(1,3),(4,3),(2,4),(5,3),(5,6),(3,1)]
--pat xs = [a+b | (a,b) <- xs]

head' :: [a] -> a
head' [] = error "Can't call head on an empty list"
head' (x:_) = x

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "The first two elements are: " ++ show x ++ " and " ++ show y

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

capital :: String -> String
capital "" = "Empty string"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

bmiTell :: (RealFloat a) => a -> a -> String  
bmiTell weight height  
    | bmi <= 18.5 = "You're underweight, you emo, you!"  
    | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"  
    | otherwise                 = "You're a whale, congratulations!"  
    where bmi = weight / height ^ 2


max' :: (Ord a) => a -> a -> a
max' a b
    | a < b = b
    | otherwise = a

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
    | a > b = GT
    | a == b = EQ
    | otherwise = LT

bmiTellBetter :: (RealFloat a) => a -> a -> String  
bmiTellBetter weight height  
    | bmi <= skinny = "You're underweight, you emo, you!"  
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= fat = "You're fat! Lose some weight, fatty!"  
    | otherwise                 = "You're a whale, congratulations!"  
    where bmi = weight / height ^ 2
-- or   (skinny, normal, fat) = (18.5, 25.0, 30.0)
          skinny = 18.5
          normal = 25.0
          fat = 30.0

initials :: String -> String -> String
initials first last = [f] ++ ". " ++ [1] ++ "."
    where (f:_) = first
          (l:_) = last
--would be shorter and cleaner if just did pattern matching in params

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- hx]
    where bmi weight height = weight / height ^ 2

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^ 2
    in  sideArea + 2 * topArea

-- can use let as an expressin anywhere:
-- 4 * (let a = 9 in a +1) + 2
-- also introduce functions in local scope:
-- [let square x = x * x in (square 5, square 3, square 2)]
-- several variables inline:
-- (let a = 100; b = 200; c = 300 in a*b*c, let foo="hey "; bar = "there" in foo ++ bar)
-- (let (a,b,c) = (1,2,3) in a+b+c) * 100
--calcBmis :: (RealFloat a) => [(a, a)] -> [a]
--calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0]

--case expressions
headcase :: [a] a
headcase xs = case xs of [] -> error "Empty list"
                         (x:_) -> x

-- case expression of pattern -> result
--                    pattern -> result

describe :: [a] -> String
describe xs = "The list is " ++ case xs of [] -> "empty."
                                           [x] -> "a singleton list."
                                           xs -> "a longer list."

describeList :: [a] -> String
describeList xs = "The list is " ++ what xs
    where what [] = "empty."
          what [x] = "a singleton list."
          what xs = "a longer list." 

