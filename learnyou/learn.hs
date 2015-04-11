double x = x + x
doubleUs x y = double (x + y)
doubleSmallNumber x = (if x > 100
                        then x
                        else x*2) + 1
conan = "Conan O'Brien"
boombang xs = [if x<10 then "BOOM" else "BANG" | x <- xs, odd x]

divisible x y = if x `mod` y == 0 then True else False

cracklepop xs = [
                if divisible x 3 && 
                    divisible x 5
                    then "CracklePop"
                else if divisible x 3 then "Crackle" 
                else if divisible x 5 then "Pop" 
                else show x | x <- xs]

--lowestdenom x = [y | y <- [2..20], divisible x y]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

countLetters s = sum [1 | _ <- s]

removeNonUppercase :: [Char] -> [Char]
removeNonUppercase s = [l | l <- s, l `elem` ['A'..'Z']]

possibleTriangles = [(x,y,z) | x <-[1..10],
                        y <-[1..10], z<-[1..10]]

sumIs x ls = [item | item<-ls, x == sum item]

rightTriangles n = [(x,y,z) | x<-[1..10],
                    y<-[1..x], z<-[1..y],
                    z^2 + y^2 == x^2,
                    x+y+z==n]
